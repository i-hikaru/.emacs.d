;; global-set-key
(global-set-key "\C-x\C-g" 'goto-line)

;; (install-elisp "http://taiyaki.org/elisp/sense-region/src/sense-region.el")
;; (require 'sense-region)
;; (sense-region-on)

;; Print-out
;; ref., http://www.gfd-dennou.org/member/morikawa/memo/emacs.txt
(require 'ps-print)
(setq ps-print-color-p t                      ; カラーを使用
      ps-multibyte-buffer 'non-latin-printer) ; 日本語使用

;; クリップボードを共有する (OSX and tmux)
; http://mogproject.blogspot.jp/2014/04/how-to-integrate-clipboard-mac-tmux.html
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
 
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
 
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; load-path
(let((default-directory (expand-file-name "~/.emacs.d/elisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

;;
;; key-bind setting
;;
;; C-hをbackspaceにする
(keyboard-translate ?\C-h ?\C-?) ; ?\C-?はDELのキーシケンス

;; C-tでウィンドウを切り替える
(define-key global-map (kbd "C-t") 'other-window)


;;; setting fram
;; カラム番号も表示する
(column-number-mode t)

;; ファイルサイズを表示する
(size-indication-mode t)

;; バッテリー残量を表示
(display-battery-mode t)

;; リージョン内の行数と文字数をモードラインに表示する
;; http://d.hatena.ne.jp/sonota88/20110224/1298557375
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
      ;; これだとエコーエリアがチラつく
      ;;(count-lines-region (region-beginning) (region-end))
    ""))

(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))

;; タイトルバーにファイルのフルパスを表示する
(setq frame-title-format "%f")


;;; 表示/装飾に関する設定
;; リージョンをハイライトする
(setq-default transient-mark-mode t)


;; C-tでウィンドウを切り替える
;(define-key global-map (kbd "C-t") 'other-window)
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p) (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)


;; C-x 3で分割したときも折り返す
(setq truncate-partial-width-windows nil)


;; 起動時のメッセージを表示しない
(setq inhibit-startup-message t)

;;
;; バッファとクリップボードを共有する (コピペ)
;; ref: http://saku-na63.hatenablog.com/entry/2013/10/23/220059
; OSXとLinuxで設定のコンフリクトを避ける
;; (setq darwin-p (eq system-type 'darwin)
;;       linux-p (eq system-type 'gnu/linux)
;;       carbon-p (eq system-type 'mac))

;; (defun copy-from-osx ()
;;   (shell-command-to-string "pbpaste"))

;; (defun paste-to-osx (text &optional push)
;;   (let ((process-connection-type nil))
;;     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
;;       (process-send-string proc text)
;;       (process-send-eof proc))))
;; (when (or darwin-p carbon-p)
;;   (setq interprogram-cut-function 'paste-to-osx)
;;   (setq interprogram-paste-function 'copy-from-osx))


;;; elip
;;auto-install
;; (when (require 'auto-install nil t)
;;   (setq auto-install-directory "~/.emacs.d/elisp/") ; インストールするディレクトリ
;;   ;; EmacsWikiに登録されているelisp の名前を取得する
;;   (auto-install-update-emacswiki-package-name t)
;;   ;; 必要であればプロキシの設定を行う
;;   ;; (setq url-proxy-services '(("http" . "localhost:8339")))
;;   ;; install-elisp の関数を利用可能にする
;;   (auto-install-compatibility-setup))


;; auto-complete-mode
(add-to-list 'load-path "~/.emacs.d/elisp")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
(ac-config-default)


;; html-helper-mode
(add-hook 'html-helper-load-hook '(lambda () (require 'html-font))) ; html-font.elを使う場合に記述
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist
      (append
       '(
	 ("\\.h$"    . c++-mode)
	 ("\\.hpp$"  . c++-mode)
	 ("\\.txt$"  . text-mode)
	 ("\\.message$" . text-mode)
	 ("\\.htm" . html-helper-mode)
	 ("\\.shtml$" . html-helper-mode)
	 ("\\.php" . html-helper-mode)
	 ) auto-mode-alist))


;; css-mode
(autoload 'css-mode "css-mode")
(setq auto-mode-alist (cons '("\\.css$" . css-mode) auto-mode-alist))


;; php-mode
(require 'php-mode)
(setq php-mode-force-pear t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))


;; YaTeX
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq YaTeX-kanji-code 3) ; euc-jp
;; forbit auto-fill-function
(add-hook 'yatex-mode-hook'(lambda ()(setq auto-fill-function nil)))
;; TeXShopでプレビューする
(setq tex-command "~/Library/TeXShop/bin/platex2pdf-euc" dvi2-command "open -a TeXShop") ; eucの場合

;;
;; aspell
;; Spell checker
;;
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
 '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
; http://keisanbutsuriya.blog.fc2.com/blog-entry-60.html
(mapc
 (lambda (hook)
   (add-hook hook 'flyspell-prog-mode))
 '( ; コメント領域のみ flyspell-mode 有効
   c-mode-common-hook
   emacs-lisp-mode-hook
   ))
(mapc
 (lambda (hook)
   (add-hook hook
	     '(lambda () (flyspell-mode 1))))
 '( ; 常に flyspell-mode 有効
   yatex-mode-hook
   markdown-mode-hook
   ))

;; auto-async-byte-compile
(require 'auto-async-byte-compile)
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)


;; screen-lines
(require 'screen-lines)
(add-hook 'text-mode-hook 'turn-on-screen-lines-mode)


;; autoinsert
(auto-insert-mode)
;; (setq auto-install-directory "~/.emacs.d/insert/")
(setq auto-insert-directory "~/.emacs.d/insert/")
(define-auto-insert "\\.py$" "python-template.py")
(define-auto-insert "\\.markdown$" "log-template.markdown")
(define-auto-insert "\\.vhd$" "vhdl-template.vhd")


;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq markdown-command "perl ~/Dropbox/perl/Markdown/Markdown.pl") ; Path of Markdown.pl
;; (setq auto-mode-alist
;;       (cons '("\\.md" . markdown-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;; evernote-mode
(require 'evernote-mode)
(setq evernote-username "i_hikaru")
(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))
(global-set-key "\C-cec" 'evernote-create-note)     ; ノートを作成
(global-set-key "\C-ceo" 'evernote-open-note)       ; ノートを開く 
(global-set-key "\C-ces" 'evernote-search-notes)    ; ノートを検索
(global-set-key "\C-ceS" 'evernote-do-saved-search) ; 検索を保存
(global-set-key "\C-cew" 'evernote-write-note)      ; ノートに書き込む
(global-set-key "\C-cep" 'evernote-post-region)     ; 選択されたリージョンを新規ノートとしてポスト
(global-set-key "\C-ceb" 'evernote-browser)         ; Evenote Browserを開く
	

;;; misc.
;; Japanese Manual
(add-to-list 'Info-default-directory-list "~/.emacs.d/info")

;; temp
(setq YaTeX-use-jmode-hook nil)
(put 'narrow-to-region 'disabled nil)

;;
;; VHDL Mode
;;
(autoload 'vhdl-mode "vhdl-mode" "VHDL Mode" t)
(setq auto-mode-alist (cons '("\\.vhdl?$" . vhdl-mode) auto-mode-alist))
;;; Customizations for VHDL Mode
(custom-set-variables
 ;; enter customizations of VHDL Mode variables here
 )
;;; Miscellaneous customizations
(custom-set-variables
 '(ps-paper-type 'a4)
 '(ps-print-color-p nil)
 '(show-paren-mode t nil (paren))
 )


;; Verilog Mode
(defun prepend-path ( my-path )
(setq load-path (cons (expand-file-name my-path) load-path)))

(defun append-path ( my-path )
(setq load-path (append load-path (list (expand-file-name my-path)))))

;;Lood first in the directory ~/elisp for elisp files
(prepend-path "~/.emacs.d/elisp")

;;Load verilog-mode only when needed
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )

;; Any files that end in .v should be in verilog mode
(setq auto-mode-alist (cons '("\\.v\\'" . verilog-mode) auto-mode-alist))
      
;; Any files in verilog mode shuold have their keywords colorized
(add-hook 'verilog-mode-hook '(lambda () (font-look-mode 1)))

;;
;; Insert datetime when push "F5"
;;
(define-key global-map [f5]
  '(lambda ()
     (interactive)
     (insert (format-time-string "%Y-%m-%d %H:%M:%S"))))

(put 'downcase-region 'disabled nil)
