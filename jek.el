(require 'org-publish)
(require 'cl)
(require 's)

(defvar jekel/blog-post-path-regex
  "^_posts/\([[:digit:]]+\)-\([[:digit:]]+\)-\([[:digit:]]+\)-\(.+\)\.org"
  "The path where jek.el assumes to find blog posts.")

(defvar jekel/permalink-styles
  '('date "/:category/:year/:month/:day/:title.html"
    'pretty "/:category/:year/:month/:day/:title/"
    'none "/:category/:title.html"))

(defun jekel/create-project ()
  "Creates a new project. Emacs will ask for the title of the new webpage."
  (interactive)
  )

(defun jekel/create-post ()
  "Creates a new post. Emacs will ask for the title of the new post."
  (interactive))

;; JEK.EL - GATHERING META DATA

(defun jekel/blog-keywords ()
  "Returns a list of keywords/tags for the current project.
Note: `base-directory` has to be defined.")

(defun jekel/blog-categories ()
  "Returns a list of categories for the current project.
Note: `base-directory` has to be defined.")

(defun jekel/blog-posts ()
  "Returns a list of blog posts with all their meta data for the current
project.

This is especially useful to iterate through posts for listings, archives,
category information, tag clouds and so on.

Note: `base-directory` has to be defined.")

(defun jekel/blog-posts-for-tag (tag)
  "Returns a list of all posts for a specific tag for the current project.")

(defun jekel/blog-posts-for-category (category)
  "Returns a list of all posts for a specific category for the current project.")

;; JEK.EL - PUBLISHING FUNCTIONS

(defun jekel/publish ()
  "Generates the documents accoding to the jek.el configuration.

When you are in a buffer with an object of a jek.el project, jek.el will crawl
up the directory structure and will load the projects configuration.

Otherwise it will ask for the jek.el project path, if it was
already configured for the current emacs session."
  (interactive))

(defun jekel/publishing-path (filename)
  "Returns the publishing path for a page")

(defun jekel/publish-asset (plist filename pub-dir)
  "Publish assets."
  (message "publish asset file: %s in %s" filename pub-dir))

(defun jekel/publish-less-stylesheets (plist filename pub-dir)
  "Publish stylesheets via `lessc`."
  (message "publish less stylesheet: %s in %s" filename pub-dir))

(defun jekel/publish-coffeescripts (plist filename pub-dir)
  "Publish coffeescripts via `coffee`."
  (message "publish coffeescript: %s in %s" filename pub-dir))

(defun s-match (regexp s)
  "Matches a regular expression for string. If the regular expression matches
the result is the matching string and each groups. If it did not match the
result is nil."
  (if (string-match regexp s)
      (let ((data-list (match-data)))
        (loop for beg = (car data-list)
              for end = (car (cdr data-list))
              while data-list
              do (setq data-list (cddr data-list))
              collect (substring s beg end)))))

(defun jekel/publish-org-to-html (plist filename pub-dir)
  "Publish an org file to HTML."
  (message "publish org: %s in %s" filename pub-dir)
  (require 'org)
  (unless (file-exists-p pub-dir)
    (make-directory pub-dir t))
  (let ((visiting (find-buffer-visiting filename)))
    (save-excursion
      (org-pop-to-buffer-same-window (or visiting (find-file filename)))
      (let* ((plist (cons :buffer-will-be-killed (cons t plist)))
             (init-buf (current-buffer))
             (init-point (point))
             (init-buf-string (buffer-string))
             (base-directory (let ((path (plist-get plist :base-directory)))
                               (if (s-matches? "[^/]$" path)
                                   (concat path "/")
                                 path)))
             (relative-path (s-replace base-directory "" filename))

             (opt-plist
              (org-export-process-option-filters
               (org-combine-plists (org-default-export-plist)
                                   plist
                                   (org-infile-export-plist))))

             (is-blog-post-p (s-matches? jekel/blog-post-path-regex
                                         relative-path))

             (post-path-data (s-match jekel/blog-post-path-regex
                                      relative-path))

             page-title
             page-url
             page-id
             page-keywords
             page-category
             page-description
             page-date

             (pub-path (if is-blog-post-p
                           (concat pub-dir (jekel/generate-permalink ))))

             (layout (if is-blog-post-p
                         (or (plist-get plist :jekel-default-blog-post-layout)
                             "default")
                       (or (plist-get plist :jekel-default-layout)
                           "default")))

             (exported-html (org-export-as-html (plist-get plist :headline-levels)
                                                nil plist 'string
                                                t pub-dir)))

        (jekel/render-layout layout
                             exported-html)

        (set-buffer init-buf)
        (when (buffer-modified-p init-buf)
          (erase-buffer)
          (insert init-buf-string)
          (save-buffer)
          (goto-char init-point))
        (unless visiting
          (kill-buffer init-buf))))))

(defun jekel/read-layout-forms (layout-filename)
  "Reads the expressions from a layout file.

When there where more than one expressions, then a list of expressions is
returned. If that list would contain only one element, that element is
returned instead."
  (let ((file-content (with-temp-buffer
                        (insert-file-contents layout-filename)
                        (s-trim (s-chomp (buffer-string)))))
        (read-beg 0)
        result)
    (while (< read-beg (length file-content))
      (let ((read-form (read-from-string file-content read-beg)))
        (push (car read-form) result)
        (setq read-beg (cdr read-form))))
    (if (and (listp result)
             (= (length result) 1))
        (car result)
      (nreverse result))))

(jekel/read-layout-forms "~/projects/jek.el/example/_layouts/test.html.el")

(defmacro jekel/render-layout (layout &rest forms)
  "Loads layout with the given name and renders the given forms into it."
  (let* ((layouts-dir "~/projects/jek.el/example/_layouts/")
         (layout-filename (concat layouts-dir layout ".html.el"))
         (layout-forms (jekel/read-layout-forms layout-filename)))
    `(let ,(cond ((and (listp forms)
                       (listp (car forms)))
                  `((yield-data (markup ,@forms))
                    (yield-type 'form)))
                 ((and (listp forms)
                       (keywordp (car forms)))
                  `((yield-data (markup ,forms))
                    (yield-type 'form)))
                 (t
                  `((yield-data ,@forms)
                    (yield-type 'string))))

       ,(cond ((and (listp layout-forms)
                    (listp (car layout-forms)))
               `(markup ,@layout-forms))
              ((and (listp layout-forms)
                    (keywordp (car layout-forms)))
               `(markup ,layout-forms))
              (t
               layout-forms)))))

;; HELPER FUNCTIONS FOR MARKUP.EL
(defun jekel/blog-post-path-helper (filename)
  "Returns the publishing path of a blog post.")

(defun jekel/asset-path-helper (filename)
  "Returns the publishing path of an asset file.")

(defun jekel/tag-cloud-helper ()
  "Renders a tag cloud.")


(provide 'jekel)
