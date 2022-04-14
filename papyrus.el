;; -*- lexical-binding: t; -*-

(require 'image-mode)
(require 'svg)
;; (require 'cl-lib) ; required already in svg library

(defcustom papyrus-step-size 50
  "Scroll step size in pixels."
  :group 'papyrus
  :type 'integer)

(defcustom papyrus-gap-height (line-pixel-height)
  "Page gap height."
  :group 'papyrus
  :type 'integer)

;; implement as macro's for setf-ability
(defmacro papyrus-overlays (&optional window)
  `(image-mode-window-get 'overlays ,window))

(defmacro papyrus-current-page (&optional window)
  ;;TODO: write documentation!
  `(image-mode-window-get 'page ,window))

(defmacro papyrus-page-overlay (page)
  `(nth (* 2 (1- ,page)) (papyrus-overlays)))

(defmacro papyrus-gap-overlay (page)
  `(nth (+ (* 2 (1- ,page)) 1) (papyrus-overlays)))

(defmacro papyrus-page-overlay-get (page prop)
  `(overlay-get (nth (* 2 (1- ,page)) (papyrus-overlays)) ,prop))

(defmacro papyrus-relative-vscroll (&optional window)
  `(image-mode-window-get 'relative-vscroll ,window))

(defun papyrus-window-end-vpos ()
  (+ (window-vscroll nil t) (window-text-height nil t)))

(defun papyrus-vscroll-to-relative (vpos)
  (/ (float (- (window-vscroll nil t) (car vpos)))
     (- (cdr vpos) (car vpos))))

(defun papyrus-relative-to-vscroll (vpos)
  (* (or (papyrus-relative-vscroll) 0) (- (cdr vpos) (car vpos))))

(defun papyrus-visible-overlays ()
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

(defun papyrus-display-page (page)
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
  (let* ((o (papyrus-page-overlay page))
         (im (overlay-get o 'display))
         (s (image-size im t))
         (w (car s))
         (h (cdr s)))
    (overlay-put o 'display `(space . (:width (,w) :height (,h))))
    (overlay-put o 'face `(background-color . "gray"))))

(defun papyrus-new-window-function (winprops)
  (if (= (buffer-size) 0)
      (let (overlays
            (pages (or (funcall papyrus-number-of-pages-function) 10))
            (win (car winprops))
            (inhibit-read-only t))

        (dotimes (i pages)
          (let ((i (1+ i)))
            (insert " ")
            (let ((po (make-overlay (1- (point)) (point))))
              (overlay-put po 'page  i)
              (overlay-put po 'window win)
              (push po overlays))

            (insert "\n ")
            (unless (= i pages)
              (let ((go (make-overlay (1- (point)) (point))))
                (overlay-put go 'gap i)
                (overlay-put go 'window win)
                (push go overlays)
                (insert "\n")))))
        (image-mode-window-put 'overlays (nreverse overlays))
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

(defun papyrus-redisplay (&optional winprops)
  (display-warning '(papyrus) "redisplay" :debug "*papyrus-debug-log*")

  ;; NOTE the `(when (listp image-mode-winprops-alist)' from
  ;; `image-mode-reapply-winprops' was removed here (but in the end might turn
  ;; out to be required)

  ;; Beware: this call to image-mode-winprops can't be optimized away, because
  ;; it not only gets the winprops data but sets it up if needed (e.g. it's used
  ;; by doc-view to display the image in a new window).
  (image-mode-winprops nil t)
  (let* ((pages (1+ (/ (length (image-mode-window-get 'overlays)) 2)))
         ;; (page-sizes (make-list pages (cons 400 800)))
         ;; (page-sizes (make-list pages (cons (- (window-text-width nil t) 200)
         ;;                                    (* 1.4 (window-text-width nil t)))))
         (page-sizes (funcall papyrus-page-sizes-function))

         (n 0)
         (vpos 0))

    (dolist (s page-sizes)
      (let* ((w (car s))
             (h (cdr s))
             (m (* 2 n))
             (o (nth m (papyrus-overlays))))
        (overlay-put o 'display `(space . (:width (,w) :height (,h))))
        (overlay-put o 'face `(background-color . "gray"))
        (overlay-put o 'vpos (cons vpos (setq vpos (+ vpos h))))

        ;; don't add gap after last page
        (unless (= n (1- (length page-sizes)))
          (setq o (nth (1+ m) (papyrus-overlays)))
          (overlay-put o 'display
                       `(space . (:width (,w) :height (,papyrus-gap-height))))
          (overlay-put o 'face `(background-color . "gray"))
          (overlay-put o 'vpos (cons vpos (setq vpos (+ vpos papyrus-gap-height))))

          (setq n (+ n 1))))))
  ;; (let ((current-page (car (image-mode-window-get 'displayed-pages))))
  (image-set-window-vscroll (let* ((p (papyrus-current-page))
                                   (vposition (papyrus-page-overlay-get
                                               (or p (progn (setf (papyrus-current-page) 1) 1))
                                               'vpos)))
                              (+ (car vposition )
                                 (papyrus-relative-to-vscroll vposition))))
  (let (displayed)
    (dolist (o (papyrus-visible-overlays))
      (papyrus-display-page o)
      (push o displayed))
    ;; (image-mode-window-put 'page (car (last displayed))) ; TODO check if possible to use 'displayed-pages
    (image-mode-window-put 'displayed-pages (reverse displayed))
    (image-mode-window-put 'visible-range (cons (papyrus-page-overlay-get (car (last displayed)) 'vpos)
                                                (papyrus-page-overlay-get (car displayed) 'vpos)))))

(defun papyrus-demo-scroll-forward (&optional backward)
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
        (when-let (d (car (cl-set-difference old new)))
          (papyrus-undisplay-page d)
          (image-mode-window-put 'displayed-pages (delete d old)))
        (when-let (d (car (cl-set-difference new old)))
          (papyrus-display-page d)
          (image-mode-window-put 'displayed-pages (append old (list d))))
        (image-mode-window-put 'visible-range (cons (papyrus-page-overlay-get (car new) 'vpos)
                                                    (papyrus-page-overlay-get (car (last new)) 'vpos)))))
    (setf (papyrus-relative-vscroll) (papyrus-vscroll-to-relative
                                      (papyrus-page-overlay-get (papyrus-current-page) 'vpos)))))

(defun papyrus-demo-scroll-backward ()
  (interactive)
  (papyrus-demo-scroll-forward t))

(define-derived-mode papyrus-demo-mode special-mode "Papyrus"
  ;; we don't use `(image-mode-setup-winprops)' because it would additionally
  ;; add `image-mode-reapply-winprops' to the
  ;; `window-configuration-change-hook', but `papyrus-redisplay' already
  ;; reapplies the vscroll, so we simply initialize the
  ;; `image-mode-winprops-alist' here, and (because ) add lines from
  ;; `image-mode-reapply-winprops' at the start of `papyrus-redisplay'.
  (add-hook 'window-configuration-change-hook 'papyrus-redisplay nil t)
  (add-hook 'image-mode-new-window-functions 'papyrus-new-window-function nil t)
  (unless (listp image-mode-winprops-alist)
    (setq image-mode-winprops-alist nil)))
  ;; (add-hook 'window-configuration-change-hook
	;;           #'image-mode-reapply-winprops nil t))
  ;; (image-mode-setup-winprops))

(when (featurep 'evil)
  (evil-define-key 'motion papyrus-demo-mode-map
    "j" 'papyrus-demo-scroll-forward
    "k" 'papyrus-demo-scroll-backward))

(defun papyrus-demo ()
  (interactive)
  (with-current-buffer (get-buffer-create "*papyrus-demo*")
    (setq cursor-type nil)
    (erase-buffer)
    (papyrus-demo-mode)
    (pop-to-buffer (current-buffer))))
