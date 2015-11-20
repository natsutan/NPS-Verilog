(use gauche.process)

(define-module npsv-dataflow
  (export-all))

(define *ch* '())
(define *top-name* "")
(define *module* '())

  
(define npsv-initialize!
  (lambda (topname)
    (set! *top-name* topname)
    (set! *module* '())
    (set! *ch* '())))

(define npsv-get-topname
  (lambda ()
    *top-name*))

(define npsv-get-ch
  (lambda ()
    *ch*))

(define npsv-get-modules
  (lambda () *module*))

(define add-module-to-top
  (lambda (module)
    (unless (member module *module*)
      (set! *module* (cons module *module*)))))


(define connect
  (lambda (src dst)
    (let ([connection (make <npsv-ch> :name (make-ch-name src dst) :src src :dst dst)]
          [src-port (find-src-port src)]
          [dst-port (find-dst-port dst)])
      (unless src-port (error "can't find src-port" (ref src 'name)))
      (unless dst-port (error "can't find dst-port" (ref dst 'name)))
        
      (add-module-to-top src)
      (add-module-to-top dst)
      (set! (ref connection 'src-port) src-port)
      (set! (ref connection 'dst-port) dst-port)
      (set! *ch* (cons connection *ch*)))))


(define make-dataflow
  (lambda ()
    (make-dot-script *top-name* *ch*)
    (let ((dot-file (string-append *top-name* ".dot"))
          (out-file (string-append *top-name* ".png")))
      (run-process (list "dot" "-Tpng"  dot-file "-o" out-file) :wait #t))))
                 
(define make-dot-script
  (lambda (name graph)
    (let ([fp (open-output-file (string-append name ".dot"))])
      (format fp "digraph ~A {\n" name)
      (format fp "graph [rankdir = LR];\n")
      (write-dot-node fp graph)
      (write-dot-dataflow fp graph)
      (format fp "}\n")
      (close-output-port fp))))
 
(define write-dot-dataflow 
  (lambda (fp graph)
    (dolist (edge graph)
            (format fp "~A\n" (ch->dot_edge edge)))))

(define write-dot-node
  (lambda (fp graph)
    (dolist (edge graph)
            (begin
              (write-node-shape-box fp (ref edge 'src))
              (write-node-shape-box fp (ref edge 'dst))
              ))))

(define write-node-shape-box
  (lambda (fp module)
    (let ([type (ref module 'type)]
          [name (ref module 'name)])
      (cond ((eq? type 'npsv-inmem)  (format fp "~A [shape = box]\n" name))
            ((eq? type 'npsv-outmem) (format fp "~A [shape = box]\n" name))
            (else 0)))))
             

(define ch->dot_edge
  (lambda (ch)
    (let ([src-module (ref ch 'src)]
          [dst-module (ref ch 'dst)])
      (string-append (ref src-module 'name) " -> " (ref dst-module 'name)))))
          



(provide "npsv-dataflow")
