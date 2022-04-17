;;; papyrus.el --- Virtual scroll display engine  -*- lexical-binding:t -*-

;; Copyright (C) 202 Free Software Foundation, Inc.

;; Author: D. L. Nicolai <dalanicolai@gmail.com>
;; Version: 1.0
;; Package-Requires: (image-mode svg)
;; Keywords: files, pdf
;; URL: https://github.com/dalanicolai/papyrus.el


;;; Commentary:

;; This package provides a virtual scroll engine for displaying books/documents.
;; The main purpose of the package is to provide a continuous scrolling feature.

;; The core functionality, i.e. the 'scroll' is provided by the
;; `papyrus--new-window-function' and `papyrus--redisplay' functions. The
;; function `papyrus--new-window-function' should be added to the
;; `image-mode-new-window-functions' while the `papyrus--redisplay' should be
;; added to the `window-configuration-change-hook' both as buffer local hook
;; functions (i.e. by passing a non-nil LOCAL argument to `add-hook'). For the
;; `image-mode-new-window-functions' to have effect, the `image-mode-winprops'
;; should be initialized by either using `image-mode-setup-winprops' (like in
;; the body of `pdf-view-mode') or by initializing the
;; `image-mode-winprops-alist' explicitly (by setting its value to nil, like in
;; the `papyrus-demo-mode' example).

;; The package is meant to be used in combination with some other package that
;; provides features to extract and manage the data from the document. An
;; example of such a file is the file `pdf-scroll.el' at URL:
;; https://github.com/dalanicolai/pdf-tools/blob/papyrus-version/lisp/pdf-scroll.el
;; `pdf-scroll.el' provides the configurations for pdf-tools to work with
;; `papyrus.el'

;; However, for development purposes, the package provides a `papyrus-demo-mode'
;; function.

;; This file provides four buffer local variables that should be set to the
;; value of the function that correctly 'retrieves' the required data from the
;; document. See their docstring and `pdf-scroll.el' for more info.


;;; Issues

;; No real issues are known when using this package in a clean way, i.e. by
;; starting from `emacs -Q', then loading this package and using it.

;; However, I have experienced some errors in redisplay when using this package
;; in combination with of vertico, marginalia and savehist. In that case
;; sometimes Emacs its `redisplay' can get a little 'confused/messed up', so
;; that a page (although never the first page but only later pages) will not
;; show directly when creating a new (second, third, ...) window. In that case
;; `redisplay' can be forced by 'activating' the minibuffer (e.g. by pressing
;; `M-x') and hiding it again. It is a weird bug, because it only happens when
;; installing those packages via `use-package', but not when teh packages are
;; installed via `package-install'. Also it seems to occur mostly when these
;; three packages are combined. Additionally, it might be might 'due to' using
;; multiple Emacs versions (you can try if the issue occurs on the other Emacs
;; version also, probably not). See
;; https://lists.gnu.org/archive/html/emacs-devel/2022-04/msg00829.html Anyway,
;; I don't know what causes the bug, but this is what I have noticed from
;; inspecting it.


;;; Developers

;; If you would like to make your package work with papyrus.el it might be handy
;; to know that, instead of using just the `vscroll' to set the scrolling
;; position of a page, this package uses the page-number and a relative-scroll
;; (the amount of scrolling from the start of a page given in units of fraction
;; of a page) to set the `vscroll' position.

;;; Code:

(require 'image-mode)
(require 'svg)
;; (require 'cl-lib) ; required already in svg library

(defcustom papyrus-step-size 50
  "Scroll step size in pixels."
  :group 'papyrus
  :type 'integer)

(defcustom papyrus-gap-height 2
  "Page gap height."
  :group 'papyrus
  :type 'integer)

(defvar-local papyrus-display-page 'papyrus-demo-display-page
  "Function that sets the overlay's display property.
The function receives the page number as a single
argument (PAGE). The function should use `(papyrus-page-overlay
PAGE)' to add the image of the page as the overlay's
display-property.")

(defvar-local papyrus-number-of-pages-function nil
  "Function that should return the total number of pages.
The function should return an integer with the total number of
pages in the document.")

(defvar-local papyrus-page-sizes-function nil
  "Function that should return page-sizes of document.
The function should return a list of conses of the form (WIDTH .
HEIGHT), both numbers.")

(defvar-local papyrus-set-redisplay-flag-function nil)

;; implement as macro's for setf-ability
(defmacro papyrus-overlays (&optional window)
  "List of overlays that make up a scroll.
Overlays with an even index hold the page-overlays, where the
overlay at index 0 holds page number 1. For each page, except for
the last page, the subsequent element holds the gap-overlay."
  `(image-mode-window-get 'overlays ,window))

(defmacro papyrus-current-page (&optional window)
  "Return the page number of the currently displayed page.
The current page is the page that overlaps with the window
start (this choice was made in order to simplify the scrolling
logic)"
  `(image-mode-window-get 'page ,window))

(defmacro papyrus-page-overlay (page)
  "Return the overlay that hold page number PAGE.
Implemented as macro to make it setf'able."
  `(nth (* 2 (1- ,page)) (papyrus-overlays)))

(defmacro papyrus-gap-overlay (page)
  "Return the overlay that holds the gap after page number PAGE.
Implemented as macro to make it setf'able."
  `(nth (+ (* 2 (1- ,page)) 1) (papyrus-overlays)))

(defmacro papyrus-page-overlay-get (page prop)
  "Get overlay-property PROP of overlay holding page number PAGE.
Implemented as macro to make it setf'able."
  `(overlay-get (nth (* 2 (1- ,page)) (papyrus-overlays)) ,prop))

(defmacro papyrus-relative-vscroll (&optional window)
  "Get vscroll from start of the page as a page fraction."
  `(image-mode-window-get 'relative-vscroll ,window))

(defun papyrus-window-end-vpos ()
  "Return vscroll position corresponding with end of window.
I.e. window-vscroll plus window-text-height in pixels."
  (+ (window-vscroll nil t) (window-text-height nil t)))

(defun papyrus-vscroll-to-relative (vpos)
  "Return scroll from beginning of page as page fraction."
  (/ (float (- (window-vscroll nil t) (car vpos)))
     (- (cdr vpos) (car vpos))))

(defun papyrus-relative-to-vscroll (&optional vpos)
  "Transform `papyrus-relative-vscroll' to pixels.
VPOS is a cons with car and cdr the starting end ending (vscroll)
position (of some page) in pixels. If no VPOS is passed or VPOS
is nil then return 0."
  (if vpos
      (* (or (papyrus-relative-vscroll) 0) (- (cdr vpos) (car vpos)))
    0))

(defun papyrus-visible-overlays ()
  "Page numbers corresponding of currently visible overlays.
The numbers are returned in a list. Overlays that are only
partially visible are included."
  (let* (visible
         flag
         (wstart (window-vscroll nil t))
         (wend (+ wstart (window-text-height nil t)))
         (overlays (papyrus-overlays)))
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

(defun papyrus-demo-display-page (page)
  "Return demo image of page.
This function is used for the papyrus-demo."
  (let* ((o (papyrus-page-overlay page))
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

(defun papyrus-undisplay-page (page)
  "Undisplay PAGE.
The function replaces the image display property of the overlay
holding PAGE with a space. It size is determined from the image
its `image-size'."
  (display-warning '(papyrus) (format "undisplay %s" page)
                   :debug "*papyrus-debug-log*")
  (let* ((o (papyrus-page-overlay page))
         (im (overlay-get o 'display))
         (s (image-size im t))
         (w (car s))
         (h (cdr s)))
    (overlay-put o 'display `(space . (:width (,w) :height (,h))))
    (overlay-put o 'face `(:background "gray"))))

(defun papyrus--new-window-function (winprops)
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
            (pages (if papyrus-number-of-pages-function
                       (funcall papyrus-number-of-pages-function)
                     25))
            (win (car winprops))
            (inhibit-read-only t))

        (erase-buffer)

        ;; here we only add the 'page' and 'window' overlay-properties, we add
        ;; more properties/information as soon as it becomes available in the
        ;; 'papyrus-display' function
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
        (when-let (fun papyrus-set-redisplay-flag-function)
          (funcall fun)))
    (let ((ols (mapcar (lambda (o)
                         (let ((oc (copy-overlay o)))
                           (overlay-put oc 'window (car winprops))
                           oc))
                       (papyrus-overlays))))
      (image-mode-window-put 'overlays ols winprops))))
      ;; NOTE I don't remember why I added the following line, it might not be
      ;; required, as generally (always?) creating a new window will also cause
      ;; the the `window-configuration-change-hook' to get called
      ;; (papyrus--redisplay))))

;; TODO find out why redisplay does not work when image height gets smaller than
;; window height (e.g. using `pdf-view-shrink'). Some unexpected behavior occurs
;; and it is not obvious to find why.
(defun papyrus--redisplay (&optional window relative-vscroll)
  (display-warning '(papyrus) (format "redisplay %s" (car (image-mode-winprops))) :debug "*papyrus-debug-log*")

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
         (page-sizes (if papyrus-page-sizes-function
                         (funcall papyrus-page-sizes-function)
                       (let ((h (window-text-height nil t)))
                         (make-list pages (cons (/ (float h) 1.4) h)))))
                       ;; (let ((w (window-pixel-width)))
                       ;;   (make-list pages (cons w (* 1.4 w))))))

         (n 0)
         (vpos 0))

    (dolist (s page-sizes)
      (let* ((w (car s))
             (h (cdr s))
             (m (* 2 n))
             (o (nth m (papyrus-overlays))))
        (overlay-put o 'display `(space . (:width (,w) :height (,h))))
        (overlay-put o 'face `(:background "gray"))
        (overlay-put o 'vpos (cons vpos (setq vpos (+ vpos h))))

        ;; don't add gap after last page
        (unless (= n (1- (length page-sizes)))
          (setq o (nth (1+ m) (papyrus-overlays)))
          (overlay-put o 'display
                       `(space . (:width (,w) :height (,papyrus-gap-height))))
          (overlay-put o 'face `(:background "gray"))
          (overlay-put o 'vpos (cons vpos (setq vpos (+ vpos papyrus-gap-height))))

          (setq n (+ n 1))))))
  ;; (let ((current-page (car (image-mode-window-get 'displayed-pages))))
  (image-set-window-vscroll (let* ((p (papyrus-current-page))
                                   (vposition (papyrus-page-overlay-get
                                               (or p (progn (setf (papyrus-current-page) 1) 1))
                                               'vpos)))
                              (+ (car vposition )
                                 (papyrus-relative-to-vscroll (unless relative-vscroll vposition)))))
  (let (displayed)
    (dolist (p (papyrus-visible-overlays))
      (funcall papyrus-display-page p)
      (push p displayed))
    ;; (image-mode-window-put 'page (car (last displayed))) ; TODO check if possible to use 'displayed-pages
    (image-mode-window-put 'displayed-pages (reverse displayed))
    (image-mode-window-put 'visible-range (cons (papyrus-page-overlay-get (car (last displayed)) 'vpos)
                                                (papyrus-page-overlay-get (car displayed) 'vpos)))))

(defun papyrus-next-page (&optional backward)
  (interactive)
  (let* ((vpos (papyrus-page-overlay-get (papyrus-current-page) 'vpos))
         (beg (car vpos))
         (end (cdr vpos)))
    (image-set-window-vscroll (funcall (if backward '- '+)
                                       (window-vscroll nil t)
                                       (- end beg)
                                       papyrus-gap-height)))
  (if backward
      (cl-decf (papyrus-current-page))
    (cl-incf (papyrus-current-page)))
  (let ((old (image-mode-window-get 'displayed-pages))
        (new (papyrus-visible-overlays)))
    (when-let (p (car (cl-set-difference old new)))
      (papyrus-undisplay-page p)
      (image-mode-window-put 'displayed-pages (setq old (delete p old))))
    (when-let (p (car (cl-set-difference new old)))
      (funcall papyrus-display-page p)
      (image-mode-window-put 'displayed-pages (append old (list p))))
    (image-mode-window-put 'visible-range (cons (papyrus-page-overlay-get (car new) 'vpos)
                                                (papyrus-page-overlay-get (car (last new)) 'vpos)))))

(defun papyrus-previous-page ()
  (interactive)
  (papyrus-next-page t))

;; this function determines the images to (un)display from comparing the visible
;; overlays(/pages) after with the displayed pages before scrolling. Because
;; `papyrus-visible-overlays' is a costly function, we don't compare on each
;; step, but we only compare after the buffer is scrolled to outside the
;; `visible-range' which is the vscroll range of the currently displayed pages.
(defun papyrus-scroll-forward (&optional backward)
  (interactive)
  (let ((new-vscroll (image-set-window-vscroll (funcall (if backward '- '+)
                                                        (window-vscroll nil t)
                                                        papyrus-step-size)))
        (visible-range (image-mode-window-get 'visible-range)))
    (when (or (progn
                (when (funcall (if backward '< '>)
                               new-vscroll (if backward
                                               (caar visible-range)
                                             (cdar visible-range)))
                  (if backward
                      (cl-decf (papyrus-current-page))
                    (cl-incf (papyrus-current-page)))))
              (funcall (if backward '< '>)
                       (papyrus-window-end-vpos) (if backward
                                                     (- (cadr visible-range) papyrus-gap-height)
                                                   (+ (cddr visible-range) papyrus-gap-height))))
      (let ((old (image-mode-window-get 'displayed-pages))
            (new (papyrus-visible-overlays)))
        (when-let (p (car (cl-set-difference old new)))
          (papyrus-undisplay-page p)
          (image-mode-window-put 'displayed-pages
                                 (setq old (delete p old)))) ;; important to update/setq old before
                                                             ;; setting/appending new below
        (when-let (p (car (cl-set-difference new old)))
          (funcall papyrus-display-page p)
          (image-mode-window-put 'displayed-pages (append old (list p))))
        ;; update also visible-range
        (image-mode-window-put 'visible-range (cons (papyrus-page-overlay-get (car new) 'vpos)
                                                    (papyrus-page-overlay-get (car (last new)) 'vpos)))))
    (setf (papyrus-relative-vscroll) (papyrus-vscroll-to-relative
                                      (papyrus-page-overlay-get (papyrus-current-page) 'vpos)))))

(defun papyrus-scroll-backward ()
  (interactive)
  (papyrus-scroll-forward t))

(define-derived-mode papyrus-demo-mode special-mode "Papyrus"
  ;; we don't use `(image-mode-setup-winprops)' because it would additionally
  ;; add `image-mode-reapply-winprops' to the
  ;; `window-configuration-change-hook', but `papyrus--redisplay' already
  ;; reapplies the vscroll, so we simply initialize the
  ;; `image-mode-winprops-alist' here, and add lines from
  ;; `image-mode-reapply-winprops' at the start of `papyrus--redisplay'.
  (add-hook 'window-configuration-change-hook 'papyrus--redisplay nil t)
  (add-hook 'image-mode-new-window-functions 'papyrus--new-window-function nil t)
  (unless (listp image-mode-winprops-alist)
    (setq image-mode-winprops-alist nil)))
  ;; (add-hook 'window-configuration-change-hook
	;;           #'image-mode-reapply-winprops nil t))
  ;; (image-mode-setup-winprops))

(setq papyrus-demo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<down>") 'papyrus-scroll-forward)
    (define-key map (kbd "<up>") 'papyrus-scroll-backward)
    (define-key map (kbd "<next>") 'papyrus-next-page)
    (define-key map (kbd "<prior>") 'papyrus-previous-page)
    map))

(when (featurep 'evil)
  (evil-define-key 'motion papyrus-demo-mode-map
    "j" 'papyrus-scroll-forward
    "k" 'papyrus-scroll-backward
    "J" 'papyrus-next-page
    "K" 'papyrus-previous-page))

(defun papyrus-demo ()
  (interactive)
  (with-current-buffer (get-buffer-create "*papyrus-demo*")
    (setq cursor-type nil)
    (erase-buffer)
    (papyrus-demo-mode)
    (switch-to-buffer (current-buffer))))

(provide 'papyrus)

;;; papyrus.el ends here
