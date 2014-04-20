(ert-deftest recursive-project-files ()
  "Test retrieving project files."
  (fakir-fake-file
   (list
    (fakir-file
     :filename "orgfile.org"
     :directory "/home/fake/blog"
     :mtime "Sun, Apr 20 2014 11:14:21 GMT")
    (fakir-file
     :filename "2014-04-20-some-post.org"
     :directory "/home/fake/blog/_posts"
     :mtime "Sun, Apr 20 2014 11:16:02 GMT"))
   (should (equal (jekel--project-files "/home/fake/blog")
                  '(("/home/fake/blog/orgfile.org" (21331 44045))
                    ("/home/fake/blog/_posts/2014-04-20-some-post.org" (21331 44146)))))))
