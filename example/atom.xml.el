(:xml
 (:feed
  (:title "My Weblog")
  (:link :href "http://www.my-weblog.com/atom.xml" :rel "self")
  (:link :href "http://www.my-weblog.com")
  (:updated (jekel-format-xml-date jekel-site-time))
  (:id "http://www.my-weblog.com")
  (:author
   (:name "Arthur Leonard Andersen")
   (:email "some@email.com"))
  (loop for )))
