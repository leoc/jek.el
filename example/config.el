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
        ("example" :components ("example-org"
                                "example-markup"
                                "example-asset"))))


(defjekel "example"
  :base-directory "~/projects/jek.el/example/"
  :publishing-directory "~/projects/jek.el/example/_site/"
  :jekel-title "An Example Weblog"
  :jekel-url "http://www.example.org"
  :jekel-default-layout "default"
  :jekel-default-blog-post-layout "post"
  :jekel-permalink-style :pretty
  :jekel-blog-archive nil
  :jekel-future-blog-posts nil
  :jekel-export-coffeescripts t
  :jekel-export-less-css t
  :jekel-commenting-engine :disqusg
  :author "Finn & Jake"
  :email "awesome@adventuretime")
