;;; jek.el --- Jekyll-like static web site generation for org-publish.

;; Copyright (c) 2012 Arthur Leonard Andersen
;;
;; Author: Arthur Leonard Andersen <leoc.git@gmail.com>
;; URL: http://github.com/leoc/jek.el
;; Version: 0.1.0
;; Keywords: Convenience, Markup, HTML
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The goal of Jekel is to simplify the generation of static websites
;; with org-mode. It features functions to help creating (nested)
;; layouts, to preprocess stylesheets and javascript and to manage
;; content objects (for example for blogs).
;;
;; Features:
;;
;;   - generation of markup via `markup.el` package
;;   - integration of Sass and Coffeescript compilers
;;   - nested layouts (define layout in org file properties)
;;
;; File mapping:
;;
;;   - `.html.el` - markup.el
;;   - `.xml.el` - markup.el
;;   - `.css.scss` - Sass (needs to be installed)
;;   - `.css.sass` - Sass (needs to be installed)
;;   - `.js.coffee` - Coffeescript (needs to be installed)
;;   - `.org` - Function: `jekel/publish-org-to-html`
;;
;; Install:
;;
;;   Add `jek.el` to your load path and add (require 'jekel) to your init.el.
;;
;; Usage:
;;
;;   To create a project call the interactive function
;; `jekel/create-project` you will be asked for the location to create
;; your new project in. A basic jekel project will be initialized
;; there.
;;
;;   When in any file that´s part of a jekel project just call
;; `jekel/compile`. It will look up the `jekel-conf.el`, load the
;; project configuration and compile all project files according the
;; the configuration.
;;
;;   When the config has been loaded via your `init.el` your project
;; has been added to the `org-publish-project-alist`. Then you can
;; simply compile via `org-publish-project`.
;;
;;; Code:
(require 'org-publish)
(require 'cl)
(require 's)
(require 'markup)

(defvar jekel/blog-post-path-regexp
  "^_posts/\\([[:digit:]]+\\)-\\([[:digit:]]+\\)-\\([[:digit:]]+\\)-\\(.+\\)\.org"
  "The path where jek.el assumes to find blog posts.")

(defvar jekel/permalink-styles
  '(:date "/:category/:year/:month/:day/:title.html"
    :pretty "/:category/:year/:month/:day/:title/"
    :none "/:category/:title.html"))

(defun jekel/create-project (project-dir project-title)
  "Creates a new project. Emacs will ask for the title of the new webpage."
  (interactive "FWhere to create jekel project? \nsWebsite title: ")
  (let* ((project-name (file-name-nondirectory project-dir))
         (project-publish-dir (concat project-dir "/_site"))
         (project-conf (format "(defjekel \"%s\"
  :base-directory \"%s\"
  :publishing-directory \"%s\"
  :title \"%s\"
  :url \"http://www.example.org\"
  :default-layout \"default\"
  :default-blog-post-layout \"post\"
  :permalink-style :pretty
  :blog-archive nil
  :future-blog-posts nil
  :export-coffeescripts t
  :export-less-css t
  :commenting-engine :disqus
  :author \"Finn & Jake\"
  :email \"awesome@adventuretime.com\")"
                       project-name
                       project-dir
                       project-publish-dir
                       project-title)))
    (unless (file-exists-p project-dir)
      (make-directory project-dir t))
    (find-file (concat project-dir "/jekel-conf.el"))
    (insert project-conf)
    (save-buffer)))

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

(defun jekel/blog-post-publishing-path (plist path-match-data)
  "Returns the filename for a specific org-file."
  (let ((category (or (plist-get plist :category) ""))
        (year (nth 1 path-match-data))
        (month (nth 2 path-match-data))
        (day (nth 3 path-match-data))
        (title (s-dashed-words (plist-get plist :title)))
        (resulting-path (plist-get jekel/permalink-styles
                                   (plist-get plist :jekel-permalink-style))))
    (setq resulting-path (s-replace ":category" category resulting-path))
    (setq resulting-path (s-replace ":year" year resulting-path))
    (setq resulting-path (s-replace ":month" month resulting-path))
    (setq resulting-path (s-replace ":day" day resulting-path))
    (setq resulting-path (s-replace ":title" title resulting-path))
    (when (s-ends-with-p "/" resulting-path)
      (setq resulting-path (concat resulting-path "index.html")))
    (substring (s-replace "//" "/" resulting-path) 1)))

(defun jekel/publish-asset (plist filename pub-dir)
  "Publish assets."
  (message "publish asset file: %s in %s" filename pub-dir))

(defun jekel/publish-less-stylesheet (plist filename pub-dir)
  "Publish stylesheets via `lessc`."
  (message "publish less stylesheet: %s in %s" filename pub-dir))

(defun jekel/publish-coffeescript (plist filename pub-dir)
  "Publish coffeescripts via `coffee`."
  (message "publish coffeescript: %s in %s" filename pub-dir))

(defun jekel/publish-markup (plist filename pub-dir)
  "Publish markup files."
  (save-excursion
    (let* ((jekel-title (plist-get plist :jekel-title))
           (jekel-url (plist-get plist :jekel-url))
           (jekel-author (plist-get plist :author))
           (jekel-email (plist-get plist :email))

           (pub-filename (expand-file-name
                          (file-name-nondirectory
                           (s-chop-suffix ".el" filename))
                          pub-dir)))

      (unless (file-exists-p pub-dir)
        (make-directory pub-dir t))

      (find-file pub-filename)
      (insert (jekel/render-markup-file filename))
      (save-buffer)
      (kill-buffer))))

(defun jekel/publish-org-to-html (plist filename pub-dir)
  "Publish an org file to HTML."
  (require 'org)
  (save-excursion
    (let ((visiting (find-buffer-visiting filename)))
      (org-pop-to-buffer-same-window (or visiting (find-file filename)))
      (let* ((plist (cons :buffer-will-be-killed (cons t plist)))
             (init-buf (current-buffer))
             (init-point (point))
             (init-buf-string (buffer-string))

             (base-directory (expand-file-name
                              (plist-get plist :base-directory)))

             (relative-path (file-relative-name filename base-directory))

             (plist
              (org-export-process-option-filters
               (org-combine-plists (org-default-export-plist)
                                   plist
                                   (org-infile-export-plist))))

             (headline-levels (plist-get plist :headline-levels))

             (blog-post-p (s-matches? jekel/blog-post-path-regexp
                                      relative-path))

             (post-path-match (s-match jekel/blog-post-path-regexp
                                       relative-path))

             (html-extension (plist-get plist :html-extension))

             (relative-pub-path (if blog-post-p
                                    (jekel/blog-post-publishing-path plist post-path-match)
                                  (concat
                                   (file-name-sans-extension
                                    (file-name-nondirectory buffer-file-name))
                                   "." html-extension)))

             (pub-dir (expand-file-name (file-name-directory relative-pub-path)
                                        (file-name-as-directory
                                         (or (if blog-post-p
                                                 (plist-get plist :publishing-directory)
                                               pub-dir)
                                             (org-export-directory :html plist)))))

             (pub-filename (file-name-nondirectory relative-pub-path))

             (pub-path (expand-file-name pub-filename
                                         pub-dir))

             (layout (or (plist-get plist :layout)
                         (if blog-post-p
                             (plist-get plist :jekel-default-blog-post-layout)
                           (plist-get plist :jekel-default-layout))
                         "default"))

             (page-id relative-pub-path)
             (jekel-title (plist-get plist :jekel-title))
             (page-title (plist-get plist :title))
             (page-author (plist-get plist :author))
             (page-url relative-pub-path)
             (page-keywords (plist-get plist :keywords))
             (page-category (plist-get plist :category))
             (page-description (plist-get plist :description))
             (page-time nil))

        (let ((exported-html (org-export-as-html headline-levels
                                                 nil plist 'string
                                                 t pub-dir)))
          (save-excursion
            (unless (file-exists-p pub-dir)
              (make-directory pub-dir t))

            (find-file pub-path)
            (insert(jekel/render-layout layout
                                         (markup-raw exported-html)))
            (save-buffer)
            (kill-buffer)))


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

(defmacro jekel/render-markup-file (markup-filename &rest forms)
  "Loads layout with the given name and renders the given forms into it.

The projects property list is scoped into this function with the name `plist`."
  (let* ((markup-forms (jekel/read-layout-forms (if (symbolp markup-filename)
                                                    (symbol-value markup-filename)
                                                  markup-filename)))
         (data-var-name (gensym)))
    `(let ,(cond ((and (listp forms)
                       (listp (car forms)))
                  `((,data-var-name (markup ,@forms))))
                 ((and (listp forms)
                       (keywordp (car forms)))
                  `((,data-var-name (markup ,forms))))
                 (t
                  `((,data-var-name ,@forms))))

       (flet ((yield () ,(cond ((and (listp forms)
                                     (listp (car forms)))
                                `(markup-raw ,data-var-name))
                               ((and (listp forms)
                                     (keywordp (car forms)))
                                `(markup-raw ,data-var-name))
                               (t
                                `,data-var-name))))

         ,(cond ((and (listp markup-forms)
                      (listp (car markup-forms)))
                 `(markup ,@markup-forms))
                ((and (listp markup-forms)
                      (keywordp (car markup-forms)))
                 `(markup ,markup-forms))
                (t
                 markup-forms))))))

(defmacro jekel/render-layout (layout-symbol &rest forms)
  "Loads layout with the given name and renders the given forms into it.

The projects property list is scoped into this function with the name `plist`."
  (let* ((layout-filename (if (symbolp layout-symbol)
                              (symbol-value layout-symbol)
                            layout-symbol))
         (layouts-dir "~/projects/jek.el/example/_layouts/")
         (layout-path (concat layouts-dir layout-filename ".html.el"))
         (layout-forms (jekel/read-layout-forms layout-path))
         (data-var-name (gensym)))
    `(let ,(cond ((and (listp forms)
                       (listp (car forms)))
                  `((,data-var-name (markup ,@forms))))
                 ((and (listp forms)
                       (keywordp (car forms)))
                  `((,data-var-name (markup ,forms))))
                 (t
                  `((,data-var-name ,@forms))))
       (flet ((yield () ,(cond ((and (listp forms)
                                     (listp (car forms)))
                                `(markup-raw ,data-var-name))
                               ((and (listp forms)
                                     (keywordp (car forms)))
                                `(markup-raw ,data-var-name))
                               (t
                                `,data-var-name))))
         ,(cond ((and (listp layout-forms)
                      (listp (car layout-forms)))
                 `(markup ,@layout-forms))
                ((and (listp layout-forms)
                      (keywordp (car layout-forms)))
                 `(markup ,layout-forms))
                (t
                 layout-forms))))))

(defun jekel/pretty-format-markup-buffer ()
  "Reformats the current buffers HTML or XML code to be much prettier.
This needn´t be done, but if you´d like to have pretty formatted code,
you can enable it in the jekel configuration.

It basically saves the excursion, switches to nxml-mode and uses the
indentation rules of nxml-mode to separate tags."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char)
      (insert "\n"))
    (indent-region begin end)))

;; HELPER FUNCTIONS FOR MARKUP.EL
(defun jekel/blog-post-path-helper (filename)
  "Returns the publishing path of a blog post.")

(defun jekel/asset-path-helper (filename)
  "Returns the publishing path of an asset file.")

(defun jekel/tag-cloud-helper ()
  "Renders a tag cloud.")


(provide 'jekel)
