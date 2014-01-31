(defun pillar-steps::faces-at-point ()
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if (listp face)
        face
      (list face))))

(defun pillar-steps::fontify ()
  (setq font-lock-fontify-buffer-function
        #'font-lock-default-fontify-buffer)
  (font-lock-fontify-buffer))

(defun pillar-steps::character-fontified-p (property valid-values)
  (pillar-steps::fontify)
  (cl-member-if
   (lambda (face)
     (memq (face-attribute face property nil t) valid-values))
   (pillar-steps::faces-at-point)))

(defun pillar-steps::character-bold-p ()
  (pillar-steps::character-fontified-p
   :weight
   '(semi-bold bold extra-bold ultra-bold)))

(Then "current point should be in bold"
  (lambda ()
    (cl-assert
     (pillar-steps::character-bold-p)
     nil
     "Expected current point to be in bold")))

(Then "current point should not be in bold"
  (lambda ()
    (cl-assert
     (not  (pillar-steps::character-bold-p))
     nil
     "Expected current point to be in bold")))

(Then "current point should be in italic"
  (lambda ()
    (cl-assert
     (pillar-steps::character-italic-p)
     nil
     "Expected current point to be in italic")))

(defun pillar-steps::character-italic-p ()
  (pillar-steps::character-fontified-p
   :slant
   '(italic oblique)))

(defun pillar-steps::character-strike-through-p ()
  (pillar-steps::character-fontified-p
   :strike-through
   '(t)))

(Then "current point should be in strike-through"
  (lambda ()
    (cl-assert
     (pillar-steps::character-strike-through-p)
     nil
     "Expected current point to be in strike-through")))

(defun pillar-steps::character-underline-p ()
  (pillar-steps::character-fontified-p
   :underline
   '(t)))

(Then "current point should be in underline"
  (lambda ()
    (cl-assert
     (pillar-steps::character-underline-p)
     nil
     "Expected current point to be in underline")))

(Then "^current point should have the \\([-a-z]+\\) face$"
  (lambda (face)
    (pillar-steps::fontify)
    (cl-assert
     (cl-member
      (intern face)
      (pillar-steps::faces-at-point)))
    nil))

(When "^I start pillar mode$"
  (lambda ()
    (pillar-mode)))

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
