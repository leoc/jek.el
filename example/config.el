(require 'org-publish)
(require 'jekel)

;; Configuring the example project
(setq org-publish-project-alist
      '(("example-org"
         :base-directory "~/projects/jek.el/example/"
         :base-extension "org"
         :recursive t
         :publishing-directory "~/projects/jek.el/example/_site/"
         :publishing-function jekel/publish-org-to-html
         :headline-levels 3
         :section-numbers nil
         :table-of-contents nil
         :author "Finn & Jake"
         :email "awesome@adventureland"
         :jekel-title "The Title Of My Blog"
         :jekel-url "http://www.example.org"
         :jekel-default-layout "default"
         :jekel-default-blog-post-layout "post"
         :jekel-permalink-style :pretty
         :jekel-blog-archive nil
         :jekel-future-blog-posts nil)
        ("example-markup"
         :base-directory "~/projects/jek.el/example/"
         :base-extension "html.el\\|xml.el"
         :recursive t
         :exclude "_layouts\\|_site\\|_posts"
         :publishing-directory "~/projects/jek.el/example/_site/"
         :publishing-function jekel/publish-markup
         :author "Finn & Jake"
         :email "awesome@adventureland"
         :jekel-title "The Title Of My Blog"
         :jekel-url "http://www.example.org"
         :jekel-default-layout "default"
         :jekel-default-blog-post-layout "post"
         :jekel-permalink-style :pretty
         :jekel-blog-archive nil
         :jekel-future-blog-posts nil)
        ("example-asset"
         :base-directory "~/projects/jek.el/example/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :recursive t
         :publishing-directory "~/projects/jek.el/example/_site/"
         :publishing-function jekel/publish-asset)
        ("example-coffee"
         :base-directory "~/projects/jek.el/example/"
         :base-extension "js.coffee"
         :recursive t
         :publishing-directory "~/projects/jek.el/example/_site/"
         :publishing-function jekel/publish-coffee)
        ("example-sass"
         :base-directory "~/projects/jek.el/example/"
         :base-extension "css.sass\\|css.scss"
         :recursive t
         :publishing-directory "~/projects/jek.el/example/_site/"
         :publishing-function jekel/publish-sass)
        ("example" :components ("example-org"
                                "example-markup"
                                "example-asset"
                                "example-coffee"
                                "example-sass"))))
