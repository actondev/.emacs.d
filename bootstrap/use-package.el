(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; enabling same use-package syntax as when using elpaca, making the keyword a no-op
(add-to-list 'use-package-keywords :elpaca)
(defun use-package-handler/:elpaca (name _keyword _arg rest state)
  (use-package-process-keywords name rest state))
