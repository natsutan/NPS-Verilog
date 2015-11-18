(define-module npsv-dataflow
  (export-all))

(define *top* '())
(define *top-name* "")

(define-class <npsv-ch> ()
  ((src :init-keyword :src)
   (dst :init-keyword :dst)
   (ch  :init-keyword :ch :init-value 0)
   (name :init-keyword :name)))
  
(define npsv-initialize!
  (lambda (topname)
    (set! *top-name* topname)
    (set! *top* '())))

(define npsv-get-topname
  (lambda ()
    *top-name*))

(define npsv-get-top
  (lambda ()
    *top*))

(define make-ch-name
  (lambda (src dst)
    (string-append (ref dst 'name) "_out")))


(define connect
  (lambda (src dst)
    (let ((connection (make <npsv-ch> :name (make-ch-name src dst) :src src :dst dst)))
      (set! *top* (cons connection *top*)))))

(define make-dataflow
  (lambda ()
    (make-dot-script *top-name* *top*)
    ))


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
