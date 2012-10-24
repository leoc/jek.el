(markup-xml
 (:xml
  (:feed
   (:title jekel-title)
   (:link :href (concat jekel-url "/atom.xml") :rel "self")
   (:link :href jekel-url)
   (:updated "time")
   (:id jekel-url)
   (:author
    (:name jekel-author)
    (:email jekel-email)))))
