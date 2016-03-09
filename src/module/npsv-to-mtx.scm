
(define-module npsv-to-mtx
  (export make-to-mtx-from-file))


(define *to-mtx-parameters*
  '(
     *npsv-row*
     *npsv-column*
     *npsv-W*
     *npsv-I*
     *npsv-module-name*
     *npsv-rtl-output-dir*
     *npsv-testbench-output-dir*
     *npsv-template-output-dir*))

(define-class <npsv-to-mtx> (<npsv-module>)
  ((row :init-keyword :row)
   (col :init-keyword :col)
   (W :init-keyword :W)
   (I :init-keyword :I)))

(define-method print-setting ((inst <npsv-to-mtx>))
  (next-method)
  (format #t "dimension ~Ax~A~%" (ref inst 'row) (ref inst 'col))
  (format #t "fixed number W ~A I ~A~%" (ref inst 'W) (ref inst 'I)))

(define make-to-mtx-from-file
 (lambda (fname)
  (clear-global-parameters! *outmnem-parameters*)
  (load-setting-file fname)
  (let ((inst (make-to-mtx-instance
               *npsv-module-name*
               *npsv-row*
               *npsv-column*
               *npsv-W*
               *npsv-I*
               *npsv-rtl-output-dir*
               *npsv-testbench-output-dir*
               *npsv-template-output-dir*)))
    (make-verilog-file inst)
    inst)))

(define make-to-mtx-instance
  (lambda (name row col W I  rtl-odir tb-odir temp-odir)
    (let ([inst (make <npsv-to-mtx> :name name :type 'npsv-to-mtx :comment "convert to mtrix module"
                      :row row :col col :W W :I I 
                      :rtl-output-dir rtl-odir :testbench-output-dir tb-odir :template-ouput-dir temp-odir)]
          [fixed (make <npsv-fixed> :W W :I I)])
      (add-port inst (make <npsv-port> :name "start" :dir 'input :comment "no use"))
      (add-port inst (make <npsv-port> :name "set" :dir 'input :comment "no use"))

      ;input
      (dolist (r (iota row))
              (dolist (c (iota col))
                      (add-port inst (make <npsv-port> :name (mtx-portname "vi_" r c) :dir 'input))
                      (add-port inst (make <npsv-port> :name (mtx-portname "fi_" r c) :dir 'input))
                      (add-port inst (make <npsv-fixed-port> :name (mtx-portname "data_i_" r c) :dir 'output :msb (- W 1) :lsb 0 :fixed-info fixed :type 'wire))))
      
      ;output
      (add-port inst (make <npsv-port> :name "vo" :dir 'output :type 'reg))
      (add-port inst (make <npsv-port> :name "fo" :dir 'output :type 'reg))
      (dolist (r (iota row))
              (dolist (c (iota col))
                      (add-port inst (make <npsv-fixed-port> :name (mtx-portname "data_o_" r c) :dir 'output :msb (- W 1) :lsb 0 :fixed-info fixed :type 'wire)) ))
      inst)))

(define-method add-top-ports ((inst <npsv-to-mtx>))
  0)

(define-method write-module-instantiation (fp (m <npsv-to-mtx>) channels)
  (let* ([name (ref m 'name)]
         [input-ch (find (lambda (ch) (eq? (ref ch 'dst) m)) channels)]
         [output-ch (find (lambda (ch) (eq? (ref ch 'src) m)) channels)]
         [input-prefix (ch->wire-prefix input-ch)]
         )
    (format fp "\t~A ~A (\n" name name)
    (write-common-connection fp)

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



;;; --------------------------------------------------------------------------------
;;; verilog rtl
;;; --------------------------------------------------------------------------------


(define-method make-verilog-file ((inst <npsv-to-mtx>))
  (define src "")
  (vsrc-add! src (vsrc-module inst))
  (vsrc-add! src (vsrc-parameter inst))
  (vsrc-add! src (vsrc-port inst))

  ;; data assign
  (dolist (r (iota (ref inst 'row)))
          (dolist (c (iota (ref inst 'col)))
                  (vsrc-add! src (format #f "\tassign ~A = ~A;\n" (mtx-portname "data_o_" r c) (mtx-portname "data_i_" r c)))))

  ;; control assign
  (let ((rclist (cartesian-product (list (iota (ref inst 'row)) (iota (ref inst 'col))))))
    (vsrc-add!
     src
     (vsrc-assign-and "vo" (map (lambda (p) (mtx-portname "vi_" (first p) (second p))) rclist)))
    (vsrc-add!
     src
     (vsrc-assign-or "fo" (map (lambda (p) (mtx-portname "fi_" (first p) (second p))) rclist))))

  
  (vsrc-add! src (vsrc-endmodule))
  (write-verilog-file inst src))

(define-method vsrc-parameter ((inst <npsv-to-mtx>))
  (format #f " # (parameter DATA_WIDTH = ~A)\n" (ref inst 'W)))


(define-method make-verilog-testbench-file ((inst <npsv-to-mtx>))
  0)


(provide "npsv-to-mtx")


