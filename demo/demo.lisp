(with-open-file (stream #P"/home/sinan/cl-mustache/demo/result.html" 
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
  (write-string (cl-mustache:mustache-render (open #P"/home/sinan/cl-mustache/demo/demo2.mustache")
                                             (yason:parse #P"/home/sinan/cl-mustache/demo/demo2.json" :object-as :alist :json-arrays-as-vectors t))
                stream))
