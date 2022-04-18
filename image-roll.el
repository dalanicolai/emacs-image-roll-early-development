;;; image-roll.el --- Virtual scroll display engine  -*- lexical-binding:t -*-

;; Copyright (C) 202 Free Software Foundation, Inc.

;; Author: D. L. Nicolai <dalanicolai@gmail.com>
;; Version: 1.0
;; Package-Requires: (image-mode svg)
;; Keywords: files, pdf
;; URL: https://github.com/dalanicolai/image-roll.el


;;; Commentary:

;; This package provides a virtual scroll engine for displaying books/documents.
;; The main purpose of the package is to provide a continuous scrolling feature.

;; The package is written in a way that it supports images of different sizes on
;; the same roll (see comment above `image-roll-scroll-forward'). Also there is
;; no minumum or maximum on the range of the sizes and finally it is written to
;; support being displayed in (any number of) multiple windows.

;; The core functionality, i.e. the 'scroll' is provided by the
;; `image-roll--new-window-function' and `image-roll--redisplay' functions. The
;; function `image-roll--new-window-function' should be added to the
;; `image-mode-new-window-functions' while the `image-roll--redisplay' should be
;; added to the `window-configuration-change-hook' both as buffer local hook
;; functions (i.e. by passing a non-nil LOCAL argument to `add-hook'). For the
;; `image-mode-new-window-functions' to have effect, the `image-mode-winprops'
;; should be initialized by either using `image-mode-setup-winprops' (like in
;; the body of `pdf-view-mode') or by initializing the
;; `image-mode-winprops-alist' explicitly (by setting its value to nil, like in
;; the `image-roll-mode' example).

;; The package is meant to be used in combination with some other package that
;; provides features to extract and manage the data from the document. An
;; example of such a file is the file `pdf-scroll.el' at URL:
;; https://github.com/dalanicolai/pdf-tools/blob/image-roll-version/lisp/pdf-scroll.el
;; `pdf-scroll.el' provides the configurations for pdf-tools to work with
;; `image-roll.el'

;; However, for development purposes, the package provides a `image-roll-demo'
;; function. Also, as an example of its usage it includes a function
;; `image-roll-directory' which can be used to view all images in some directory
;; using the roll.

;; This file provides four buffer local variables that should be set to the
;; values of the functions that correctly 'retrieve' the required data from the
;; document. See their docstrings and `image-roll-directory' (or
;; `pdf-scroll.el') for more info.


;;; Issues

;; No real issues are known when using this package in a clean way, i.e. by
;; starting from `emacs -Q', then loading this package and using it.

;; However, I have experienced some errors in redisplay when using this package
;; in combination with vertico, marginalia and savehist. In that case sometimes
;; Emacs its `redisplay' can get a little 'confused/messed up', so that a page
;; (although never the first page but only later pages) will not show directly
;; when creating a new (second, third, ...) window. In that case `redisplay' can
;; be forced by 'activating' the minibuffer (e.g. by pressing `M-x') and hiding
;; it again. It is a weird bug, because it only happens when installing those
;; packages via `use-package', but not when the packages are installed via
;; `package-install'. Also it seems to occur mostly when these three packages
;; are combined. Additionally, it might be might 'due to' using multiple Emacs
;; versions (you can try if the issue occurs on the other Emacs version also,
;; probably not). See
;; https://lists.gnu.org/archive/html/emacs-devel/2022-04/msg00829.html Anyway,
;; I don't know what causes the bug, but this is what I have noticed from
;; inspecting it.


;;; Developers

;; If you would like to make your package work with image-roll.el it might be
;; handy to know that, instead of using just the `vscroll' to set the scrolling
;; position of a page, this package uses the page-number and a relative-scroll
;; (the amount of scrolling from the start of a page given in units of fraction
;; of a page) to set the `vscroll' position (see the
;; `image-roll-set-window-vscroll' form in the `image-roll--redisplay'
;; function).

;;; Code:

(require 'image-mode)
(require 'svg)
;; (require 'cl-lib) ; required already in svg library

(defcustom image-roll-step-size 50
  "Scroll step size in pixels."
  :group 'image-roll
  :type 'integer)

(defcustom image-roll-gap-height 2
  "Page gap height."
  :group 'image-roll
  :type 'integer)

(defvar-local image-roll-number-of-pages-function nil
  "Function that should return the total number of pages.
The function should return an integer with the total number of
pages in the document.")

(defvar-local image-roll-page-sizes-function nil
  "Function that should return page-sizes of document.
The function should return a list of conses of the form (WIDTH .
HEIGHT), both numbers.")

(defvar-local image-roll-display-page 'image-roll-demo-display-page
  "Function that sets the overlay's display property.
The function receives the page number as a single
argument (PAGE). The function should use `(image-roll-page-overlay
PAGE)' to add the image of the page as the overlay's
display-property.")

(defvar-local image-roll-set-redisplay-flag-function nil)

(defvar-local image-roll-demo-page-size (let ((h (window-text-height nil t)))
                                          (cons (/ (float h) 1.4) h))
  "Page size in image-roll-demo.
The value should be a cons of the form (WIDTH . HEIGHT) with
WIDTH and HEIGHT both integers.")

;; define as macro's for setf-ability
(defmacro image-roll-overlays (&optional window)
  "List of overlays that make up a scroll.
Overlays with an even index hold the page-overlays, where the
overlay at index 0 holds page number 1. For each page, except for
the last page, the subsequent element holds the gap-overlay."
  `(image-mode-window-get 'overlays ,window))

(defmacro image-roll-current-page (&optional window)
  "Return the page number of the currently displayed page.
The current page is the page that overlaps with the window
start (this choice was made in order to simplify the scrolling
logic)"
  `(image-mode-window-get 'page ,window))

(defmacro image-roll-page-overlay (page)
  "Return the overlay that hold page number PAGE.
Implemented as macro to make it setf'able."
  `(nth (* 2 (1- ,page)) (image-roll-overlays)))

(defmacro image-roll-gap-overlay (page)
  "Return the overlay that holds the gap after page number PAGE.
Implemented as macro to make it setf'able."
  `(nth (+ (* 2 (1- ,page)) 1) (image-roll-overlays)))

(defmacro image-roll-page-overlay-get (page prop)
  "Get overlay-property PROP of overlay holding page number PAGE.
Implemented as macro to make it setf'able."
  `(overlay-get (nth (* 2 (1- ,page)) (image-roll-overlays)) ,prop))

(defmacro image-roll-relative-vscroll (&optional window)
  "Get vscroll from start of the page as a page fraction."
  `(image-mode-window-get 'relative-vscroll ,window))

;; Substitute for `image-set-window-vscroll'. This function echos 'End of
;; document' when trying to scroll beyond the end of the document (it scrolls to
;; the end of the document instead.)
(defun image-roll-set-window-vscroll (vscroll)
  (let ((max-vscroll (- (cdr (overlay-get (car (last (image-roll-overlays))) 'vpos))
                        (window-text-height nil t))))
    (cond ((> vscroll max-vscroll)
           (set-window-vscroll (selected-window) max-vscroll t)
           (user-error "End of document"))
          (t
           (setf (image-mode-window-get 'vscroll) vscroll)
           (set-window-vscroll (selected-window) vscroll t)))))

(defun image-roll-window-end-vpos ()
  "Return vscroll position corresponding with end of window.
I.e. window-vscroll plus window-text-height in pixels."
  (+ (window-vscroll nil t) (window-text-height nil t)))

(defun image-roll-vscroll-to-relative (vpos)
  "Return scroll from beginning of page as page fraction."
  (/ (float (- (window-vscroll nil t) (car vpos)))
     (- (cdr vpos) (car vpos))))

(defun image-roll-relative-to-vscroll (&optional vpos)
  "Transform `image-roll-relative-vscroll' to pixels.
VPOS is a cons with car and cdr the starting end ending (vscroll)
position (of some page) in pixels. If no VPOS is passed or VPOS
is nil then return 0."
  (if vpos
      (* (or (image-roll-relative-vscroll) 0) (- (cdr vpos) (car vpos)))
    0))

(defun image-roll-visible-overlays ()
  "Page numbers corresponding of currently visible overlays.
The numbers are returned in a list. Overlays that are only
partially visible are included."
  (let* (visible
         flag
         (wstart (window-vscroll nil t))
         (wend (+ wstart (window-text-height nil t)))
         (overlays (image-roll-overlays)))
    (while overlays
      (let* ((o (car overlays))
             (vpos (overlay-get o 'vpos))
             (pstart (car vpos))
             (pend (cdr vpos)))
        (when (and (<= pstart wstart)
                   (> pend wstart))
          (setq flag t))
        (if (and (< pstart wend)
                 (>= pend wend))
            (setq overlays nil)
          (setq overlays (cdr overlays)))
        (when (and flag
                   (overlay-get o 'page))
          (push (overlay-get o 'page) visible))))
    (nreverse visible)))

(defun image-roll-undisplay-page (page)
  "Undisplay PAGE.
The function replaces the image display property of the overlay
holding PAGE with a space. It size is determined from the image
its `image-size'."
  (display-warning '(image-roll) (format "undisplay %s" page)
                   :debug "*image-roll-debug-log*")
  (let* ((o (image-roll-page-overlay page))
         (im (overlay-get o 'display))
         (s (image-size im t))
         (w (car s))
         (h (cdr s)))
    (overlay-put o 'display `(space . (:width (,w) :height (,h))))
    (overlay-put o 'face `(:background "gray"))))

(defun image-roll--new-window-function (winprops)
  "Function called first after displaying buffer in a new window.
If the buffer is newly created, then it does not contain any
overlay and this function creates erases the buffer contents
after which it inserts empty spaces that each holds a page or gap
overlay. If the buffer already has overlays (i.e. a second or
subsequent window is created), the function simply copies the
overlays and adds the new window as window overlay-property to
each overlay."
  ;; (if (= (buffer-size) 0)
  (if (not (overlays-at 1))
      (let (overlays
            (pages (if image-roll-number-of-pages-function
                       (funcall image-roll-number-of-pages-function)
                     25))
            (win (car winprops))
            (inhibit-read-only t))

        (erase-buffer)

        ;; here we only add the 'page' and 'window' overlay-properties, we add
        ;; more properties/information as soon as it becomes available in the
        ;; 'image-roll-display' function
        (dotimes (i pages)
          (let ((i (1+ i)))
            (insert " ")
            (let ((po (make-overlay (1- (point)) (point))))
              (overlay-put po 'page  i)
              (overlay-put po 'window win)
              (push po overlays))

            (insert "\n")
            (insert (propertize " " 'face '(:height 1)))
            (unless (= i pages)
              (let ((go (make-overlay (1- (point)) (point))))
                (overlay-put go 'gap i)
                (overlay-put go 'window win)
                (push go overlays)
                (insert (propertize "\n" 'face '(:height 1)))))))
        (image-mode-window-put 'overlays (nreverse overlays))
        (set-buffer-modified-p nil)
        ;; we must put the cursor at the `point-min' for the vscroll
        ;; functionality to work. It is only required here because we will never
        ;; move the cursor (we will merely update overlay properties)
        (goto-char (point-min))
        ;; required to make `pdf-view-redisplay-some-windows' call `pdf-view-redisplay'
        (when-let (fun image-roll-set-redisplay-flag-function)
          (funcall fun)))
    (let ((ols (mapcar (lambda (o)
                         (let ((oc (copy-overlay o)))
                           (overlay-put oc 'window (car winprops))
                           oc))
                       (image-roll-overlays))))
      (image-mode-window-put 'overlays ols winprops))))
;; NOTE I don't remember why I added the following line, it might not be
;; required, as generally (always?) creating a new window will also cause
;; the the `window-configuration-change-hook' to get called
;; (image-roll--redisplay))))

(defun image-roll--redisplay (&optional window no-relative-vscroll)
  "Redisplay the scroll.
Besides that this function can be called directly, it should also
be added to the `window-configuration-change-hook'.

The argument WINDOW is not use in the body, but it exists to make
the function compatible with `pdf-tools' (in which case is a
substitute for `pdf-view-redisplay').

When NO-RELATIVE-SCROLL is non-nil, then the relative-scroll is
not included when setting teh vscroll position. For example this
is used in `pdf-view-goto-page' (in the `pdf-scroll.el'
extension) to make it scroll to the start of the page."
  (display-warning '(image-roll) (format "redisplay %s" (car (image-mode-winprops))) :debug "*image-roll-debug-log*")

  ;; NOTE the `(when (listp image-mode-winprops-alist)' from
  ;; `image-mode-reapply-winprops' was removed here (but in the end might turn
  ;; out to be required)

  ;; Beware: this call to image-mode-winprops can't be optimized away, because
  ;; it not only gets the winprops data but sets it up if needed (e.g. it's used
  ;; by doc-view to display the image in a new window).
  (image-mode-winprops nil t)
  (let* ((pages (1+ (/ (length (image-mode-window-get 'overlays)) 2)))
         ;; (page-sizes (make-list pages (cons (- (window-text-width nil t) 200)
         ;;                                    (* 1.4 (window-text-width nil t)))))
         (page-sizes (if image-roll-page-sizes-function
                         (funcall image-roll-page-sizes-function)
                       (make-list pages image-roll-demo-page-size)))
         ;; (let ((w (window-pixel-width)))
         ;;   (make-list pages (cons w (* 1.4 w))))))

         (n 0)
         (vpos 0))

    (dolist (s page-sizes)
      (let* ((w (car s))
             (h (cdr s))
             (m (* 2 n))
             (o (nth m (image-roll-overlays))))
        (overlay-put o 'display `(space . (:width (,w) :height (,h))))
        (overlay-put o 'face `(:background "gray"))
        (overlay-put o 'vpos (cons vpos (setq vpos (+ vpos h))))

        ;; don't add gap after last page
        (unless (= n (1- (length page-sizes)))
          (setq o (nth (1+ m) (image-roll-overlays)))
          (overlay-put o 'display
                       `(space . (:width (,w) :height (,image-roll-gap-height))))
          (overlay-put o 'face `(:background "gray"))
          (overlay-put o 'vpos (cons vpos (setq vpos (+ vpos image-roll-gap-height))))

          (setq n (+ n 1))))))
  ;; (let ((current-page (car (image-mode-window-get 'displayed-pages))))
  (image-roll-set-window-vscroll (let* ((p (image-roll-current-page))
                                        (vposition (image-roll-page-overlay-get
                                                    (or p (progn (setf (image-roll-current-page) 1) 1))
                                                    'vpos)))
                                   (+ (car vposition )
                                      (image-roll-relative-to-vscroll (unless no-relative-vscroll vposition)))))
  (let (displayed)
    (dolist (p (image-roll-visible-overlays))
      (funcall image-roll-display-page p)
      (push p displayed))
    ;; (image-mode-window-put 'page (car (last displayed))) ; TODO check if possible to use 'displayed-pages
    (image-mode-window-put 'displayed-pages (reverse displayed))
    (image-mode-window-put 'visible-range (cons (image-roll-page-overlay-get (car (last displayed)) 'vpos)
                                                (image-roll-page-overlay-get (car displayed) 'vpos)))))

(defun image-roll-next-page (&optional backward)
  (interactive)
  (let* ((vpos (image-roll-page-overlay-get (image-roll-current-page) 'vpos))
         (beg (car vpos))
         (end (cdr vpos)))
    (image-roll-set-window-vscroll (funcall (if backward '- '+)
                                            (window-vscroll nil t)
                                            (- end beg)
                                            image-roll-gap-height)))
  (if backward
      (cl-decf (image-roll-current-page))
    (cl-incf (image-roll-current-page)))
  (let ((old (image-mode-window-get 'displayed-pages))
        (new (image-roll-visible-overlays)))
    ;; (when-let (p (car (cl-set-difference old new)))
    (dolist (p (cl-set-difference old new))
      (image-roll-undisplay-page p)
      (image-mode-window-put 'displayed-pages (setq old (delete p old))))
    ;; (when-let (p (car (cl-set-difference new old)))
    (dolist (p (cl-set-difference new old))
      (funcall image-roll-display-page p)
      (image-mode-window-put 'displayed-pages (append old (list p))))
    (image-mode-window-put 'visible-range (cons (image-roll-page-overlay-get (car new) 'vpos)
                                                (image-roll-page-overlay-get (car (last new)) 'vpos)))))

(defun image-roll-previous-page ()
  (interactive)
  (image-roll-next-page t))

;; this function determines the images to (un)display from comparing the visible
;; overlays(/pages) after with the displayed pages before scrolling. Because
;; `image-roll-visible-overlays' is a costly function, we don't compare on each
;; step, but we only compare after the buffer is scrolled to outside the
;; `visible-range' which is the vscroll range of the currently displayed pages.
(defun image-roll-scroll-forward (&optional backward screen)
  (interactive)
  (let ((new-vscroll (image-roll-set-window-vscroll (funcall (if backward '- '+)
                                                             (window-vscroll nil t)
                                                             (if screen
                                                                 (window-text-height nil t)
                                                               image-roll-step-size))))
        (visible-range (image-mode-window-get 'visible-range)))
    (when (or (progn
                (when (funcall (if backward '< '>)
                               new-vscroll (if backward
                                               (caar visible-range)
                                             (cdar visible-range)))
                  (if backward
                      (cl-decf (image-roll-current-page))
                    (cl-incf (image-roll-current-page)))))
              (funcall (if backward '< '>)
                       (image-roll-window-end-vpos) (if backward
                                                        (- (cadr visible-range) image-roll-gap-height)
                                                      (+ (cddr visible-range) image-roll-gap-height))))
      (let ((old (image-mode-window-get 'displayed-pages))
            (new (image-roll-visible-overlays)))
        ;; dolist because if images/pages are small enough, there might be
        ;; multiple image that need to get updated
        (dolist (p (cl-set-difference old new))
          (image-roll-undisplay-page p)
          (image-mode-window-put 'displayed-pages
                                 (setq old (delete p old)))) ;; important to update/setq old before
                                                             ;; setting/appending new below
        (dolist (p (cl-set-difference new old))
          (funcall image-roll-display-page p)
          (image-mode-window-put 'displayed-pages (setq old (append old (list p)))))
        ;; update also visible-range
        (image-mode-window-put 'visible-range (cons (image-roll-page-overlay-get (car new) 'vpos)
                                                    (image-roll-page-overlay-get (car (last new)) 'vpos)))))
    (setf (image-roll-relative-vscroll) (image-roll-vscroll-to-relative
                                         (image-roll-page-overlay-get (image-roll-current-page) 'vpos)))))

(defun image-roll-scroll-backward ()
  (interactive)
  (image-roll-scroll-forward t))

(defun image-roll-next-screen ()
  (interactive)
  (image-roll-scroll-forward nil t))

(defun image-roll-previous-screen ()
  (interactive)
  (image-roll-scroll-forward t t))

;;; Demo

(defun image-roll-demo-display-page (page)
  "Return demo image of page.
This function is used for the image-roll-demo."
  (let* ((o (image-roll-page-overlay page))
         (s (cdr (overlay-get o 'display)))
         (w (car (plist-get s :width)))
         (h (car (plist-get s :height)))
         (svg (svg-create w h)))
    (svg-rectangle svg 0 0 w h :fill-color "white")
    (svg-text svg
              (number-to-string page)
              :font-size "40"
              :fill "black"
              :x 20
              :y 50)
    (overlay-put o 'display (svg-image svg))))

(define-derived-mode image-roll-mode special-mode "Papyrus"
  ;; we don't use `(image-mode-setup-winprops)' because it would additionally
  ;; add `image-mode-reapply-winprops' to the
  ;; `window-configuration-change-hook', but `image-roll--redisplay' already
  ;; reapplies the vscroll, so we simply initialize the
  ;; `image-mode-winprops-alist' here, and add lines from
  ;; `image-mode-reapply-winprops' at the start of `image-roll--redisplay'.
  (add-hook 'window-configuration-change-hook 'image-roll--redisplay nil t)
  (add-hook 'image-mode-new-window-functions 'image-roll--new-window-function nil t)
  (unless (listp image-mode-winprops-alist)
    (setq image-mode-winprops-alist nil)))
;; (add-hook 'window-configuration-change-hook
;;           #'image-mode-reapply-winprops nil t))
;; (image-mode-setup-winprops))

(setq image-roll-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "<down>") 'image-roll-scroll-forward)
        (define-key map (kbd "<up>") 'image-roll-scroll-backward)
        (define-key map (kbd "<next>") 'image-roll-next-page)
        (define-key map (kbd "<prior>") 'image-roll-previous-page)
        (define-key map (kbd "S-<next>") 'image-roll-next-screen)
        (define-key map (kbd "S-<prior>") 'image-roll-previous-screen)
        map))

(when (featurep 'evil)
  (evil-define-key 'motion image-roll-mode-map
    "j" 'image-roll-scroll-forward
    "k" 'image-roll-scroll-backward
    "J" 'image-roll-next-page
    "K" 'image-roll-previous-page
    (kbd "C-S-j") 'image-roll-next-screen
    (kbd "C-S-k") 'image-roll-previous-screen))

(defun image-roll-demo (&optional page-size)
  (interactive)
  (with-current-buffer (get-buffer-create "*image-roll-demo*")
    (erase-buffer)
    (image-roll-mode)
    (setq cursor-type nil)
    (when page-size (setq image-roll-demo-page-size page-size))
    (switch-to-buffer (current-buffer))))

;;; Directory image roll

(defvar-local image-mode-directory nil)

(defun image-roll-directory-image-files (dir)
  (let (dir-image-data)
    (dolist (f (directory-files dir))
      (when (image-supported-file-p f)
        (push (concat (file-name-as-directory dir) f) dir-image-data)))
    dir-image-data))

(defun image-roll-directory-image-sizes (&optional dir)
  (mapcar (lambda (f)
            (image-size (create-image f) t))
          (image-roll-directory-image-files (or dir image-mode-directory))))

(defun image-roll-number-of-images (&optional dir)
  (length (image-roll-directory-image-files (or dir image-mode-directory))))

(defun image-roll-dir-image (i &optional dir)
  (let ((image (create-image (nth (- i 1) (image-roll-directory-image-files dir)))))
    image))

;; (image-roll-dir-image 1 "~/Pictures")

(defun image-roll-dir-display-page (i &optional dir)
  (overlay-put (image-roll-page-overlay i)
               'display (image-roll-dir-image i (or dir image-mode-directory))))

(defun image-roll-directory (dir)
  (interactive "D")
  (with-current-buffer (get-buffer-create "*image-roll-directory*")
    (erase-buffer)
    (image-roll-mode)
    (setq image-mode-directory dir)
    (setq image-roll-number-of-pages-function 'image-roll-number-of-images)
    (setq image-roll-page-sizes-function 'image-roll-directory-image-sizes)
    (setq image-roll-display-page 'image-roll-dir-display-page)
    (setq cursor-type nil)
    (switch-to-buffer (current-buffer))))

(provide 'image-roll)

;;; image-roll.el ends here
