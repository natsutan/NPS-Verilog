(define-module npsv-portconv
  (export-all))


(define-class <npsv-portconv> (<npsv-module>)
  ((src :init-keyword :src)
   (dst :init-keyword :dst)))
        

(define-method make-portconv ((src <npsv-fixed-port>) (dst <npsv-adr-port>) name dir)
  (let ([inst (make <npsv-portconv> :type 'npsv-portconv :name name :src src :dst dst
                    :rtl-output-dir dir)]
        [new-src (copy-instance src)]
        [new-dst (copy-instance dst)]
        )
    (set! (ref inst 'ports) '())
    
    (add-port inst (make <npsv-port> :name "vi" :dir 'input))
    (add-port inst (make <npsv-port> :name "fi" :dir 'input))
    (add-port inst (make <npsv-port> :name "vo" :dir 'output))
    (add-port inst (make <npsv-port> :name "fo" :dir 'output))

    (set! (ref new-src 'name) "datai")
    (set! (ref new-src 'dir) 'input)
    (set! (ref new-dst 'name) "datao")
    (set! (ref new-dst 'dir) 'output)
    
    (add-port inst new-src)
    (add-port inst new-dst)

    inst))


(define-method make-verilog-file ((inst <npsv-portconv>))
  (write-verilog-file inst (make-verilog-str inst)))


(define make-verilog-str
  (lambda (inst)
    (string-append
     (format #f "module ~A \n" (ref inst 'name))
     "(\n"

     (apply string-append (portconv-ports-str (ref inst 'ports)))
                              
     ");\n"
     "\tassign vo = vi;\n"
     "\tassign fo = fi;\n"
     (make-portconv-assign-str inst)
     "\n"
     "endmodule\n"
    )))


(define portconv-ports-str
  (lambda (ports)
    (let* ([p (car ports)]
           [dir (ref p 'dir)]
           [name (ref p 'name)]
           [bit (bit-slice-str (ref p 'lsb) (ref p 'msb))])
      (if (= (length ports) 1)
          (cons (format #f "\t~A ~A ~A~%" dir bit name) '())
          (cons (format #f "\t~A ~A ~A,~%" dir bit name)
                (portconv-ports-str (cdr ports)))))))
     

(define make-portconv-assign-str
  (lambda (inst)
    (portconv-assign-str (ref inst 'src) (ref inst 'dst))))


(define-method portconv-assign-str ((src <npsv-fixed-port>) (dst <npsv-adr-port>))
  (let* ([src-name (ref src 'name)]
         [dst-name (ref dst 'name)]
         [src-w (ref-W src)]
         [dst-w (ref dst 'msb)]
         [offset (power 2 src-w)]
         )
    (string-append
     "\n"
     "\t// convert offset binary\n"
     (format #f "\twire [~A:0] tmp;\n" src-w)
     (format #f "\tassign tmp = ~A + ~A;\n" src-name offset)
     (format #f "\tassign ~A = tmp[~A:~A];~%" dst-name src-w (- src-w dst-w)))))
    
        


(define make-pconv-name
  (lambda (src dst)
    (string-append "pconv_" (ref src 'name) "_to_" (ref dst 'name))))

    

(define-method write-module-instantiation (fp (m <npsv-portconv>) channels)
  (let* ([name (ref m 'name)]
         [output-ch (find (lambda (ch) (eq? (ref ch 'src) m)) channels)]
         [input-ch (find (lambda (ch) (eq? (ref ch 'dst) m)) channels)]
         )
    (format fp "\t~A ~A (\n" name name)

    (when (not input-ch)
      (format #t "Error:no input ~A~%" name))

    (write-inport-assign fp "vi" "vo" input-ch)
    (write-inport-assign fp "fi" "fo" input-ch)
    (write-inport-assign fp "datai" "datao" input-ch)
    
    (when (not output-ch)
      (format #t "Error:no output ~A~%" name))

    (write-outport-assign fp "vo" output-ch)
    (write-outport-assign fp "fo" output-ch)
    (write-outport-assign fp "datao" output-ch :last-flag #t)

    (format fp "\t);\n");
    ))



(provide "portconv")


