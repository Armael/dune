  $ $JBUILDER runtest -j1 --root .
      ocamldep main.depends.ocamldep-output
        ocamlc bar.{cmi,cmo,cmt}
        ocamlc foo.{cmi,cmo,cmt}
      ocamlopt bar.{cmx,o}
        ocamlc main.{cmi,cmo,cmt}
      ocamlopt foo.{cmx,o}
      ocamlopt main.{cmx,o}
      ocamlopt main.exe
          main alias runtest
  bar has unix
  foo has no fake
