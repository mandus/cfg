(in-package #:nyxt-user)

;; custom swank, since I usually have others running
(setf nyxt:*swank-port* 4007)

(define-configuration nyxt/web-mode:web-mode ((nyxt/web-mode:hints-alphabet "fjdkslgrthyuieowpnvcmx")))

;; vi-bindings for prompt; general vi-bindings set in the auto-config
(define-configuration prompt-buffer ((default-modes (append '(vi-normal-mode) %slot-default%))))

;; styles - copied/slightly adapted from @aartaka

(defparameter *blue* "#7587A6")
(defparameter *grey* "#1f1e1e")

(define-configuration window
  ((message-buffer-style
    (str:concat
     %slot-default%
     (cl-css:css
      `((body
         :background-color ,*grey*
         :color "white")))))))

(define-configuration prompt-buffer
  ((style (str:concat
           %slot-default%
           (cl-css:css
            `((body
               :background-color ,*grey*
               :color "white")
              ("#prompt-area"
               :background-color ,*blue*)
              ("#input"
               :background-color ,*grey*
               :border "2px solid #1f1e1e"
               :border-radius "3px"
               :color "white")
              (".source-name"
               :color "white"
               :background-color ,*grey*)
              (".source-content"
               :background-color  ,*grey*)
              (".source-content th"
               :border-left  "3px solid #7c4f8c"
               :background-color ,*grey*)
              ("#selection"
               :background-color ,*blue*
               :color "white")
              (.marked :background-color ,*blue*
                       :font-weight "bold"
                       :color "black")
              (.selected :background-color ,*blue*
                         :color "white")))))))

(define-configuration internal-buffer
  ((style
    (str:concat
     %slot-default%
     (cl-css:css
      `((title
         :color "white")
        (body
         :background-color ,*grey*
         :color "lightgray")
        (hr
         :color "darkgray")
        (a
         :color "#556B2F")
        (.button
         :color white
         :border-radius "5px"
         :padding "0.3vw"
         :text-align "center"
         :background-color ,*blue*)))))))

(define-configuration nyxt/history-tree-mode:history-tree-mode
  ((nyxt/history-tree-mode::style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
         :background-color "black"
         :color "lightgray")
        (hr
         :color "darkgray")
        (a
         :color "white")
        ("ul li::before"
         :background-color "white")
        ("ul li::after"
         :background-color "white")
        ("ul li:only-child::before"
         :background-color "white")))))))

(define-configuration nyxt/web-mode:web-mode
  ((nyxt/web-mode:highlighted-box-style
    (cl-css:css
     '((".nyxt-hint.nyxt-highlight-hint"
        :background "white")))
    :documentation "The style of highlighted boxes, e.g. link hints.")))

(define-configuration status-buffer
  ((glyph-mode-presentation-p t)))

(define-configuration nyxt/reduce-tracking-mode:reduce-tracking-mode
  ((glyph "🛤")))

(define-configuration nyxt/emacs-mode:emacs-mode 
  ((glyph "🐮")))

(define-configuration nyxt/certificate-exception-mode:certificate-exception-mode
  ((glyph "🕵")))

(define-configuration nyxt/help-mode:help-mode
  ((glyph "❓")))

(define-configuration nyxt/web-mode:web-mode
  ((glyph "🕸")))

(define-configuration status-buffer
  ((style (str:concat
           %slot-default%
           (cl-css:css
            `(("#controls"
               :background-color ,*blue*
               :border-top "1px solid white")
              ("#url"
               :background-color ,*grey*
               :color "white"
               :border-top "1px solid white")
              ("#modes"
               :background-color ,*blue*
               :border-top "1px solid white")
              ("#tabs"
               :background-color ,*grey* 
               :color "black"
               :border-top "1px solid white")
              (".tab"
               :border-left "3px solid #7c4f8c"
               :border-radius "3px")
              (".tab:hover"
               :color ,*blue*)
              (".button:hover"
               :color ,*blue*)))))))
