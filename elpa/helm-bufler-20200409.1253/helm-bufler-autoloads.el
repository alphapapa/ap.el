;;; helm-bufler-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-bufler" "helm-bufler.el" (0 0 0 0))
;;; Generated autoloads from helm-bufler.el

(defvar helm-bufler-source (helm-make-source "Bufler's workspace buffers" 'helm-source-sync :header-name (lambda (_name) (concat "Bufler" (unless current-prefix-arg (concat ": " (bufler-format-path (frame-parameter nil 'bufler-workspace-path)))))) :candidates (lambda nil (let* ((bufler-vc-state nil) (group-path (unless current-prefix-arg (if (car (frame-parameter nil 'bufler-workspace-path)) (frame-parameter nil 'bufler-workspace-path) (cdr (frame-parameter nil 'bufler-workspace-path)))))) (bufler-buffer-alist-at group-path))) :action (cons (cons "Switch to buffer with Bufler" 'helm-bufler-switch-buffer) helm-type-buffer-actions)) "\
Helm source for `bufler'.")

(register-definition-prefixes "helm-bufler" '("helm-bufler-switch-buffer"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-bufler-autoloads.el ends here
