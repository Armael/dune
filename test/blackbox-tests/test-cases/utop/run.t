  $ dune utop --display short forutop -- init_forutop.ml
      ocamldep forutop/.utop/.utop.eobjs/utop.ml.d
      ocamldep forutop/.forutop.objs/forutop.ml.d
        ocamlc forutop/.forutop.objs/byte/forutop.{cmi,cmo,cmt}
        ocamlc forutop/forutop.cma
        ocamlc forutop/.utop/.utop.eobjs/byte/utop.{cmi,cmo,cmt}
        ocamlc forutop/.utop/utop.exe
  hello in utop
