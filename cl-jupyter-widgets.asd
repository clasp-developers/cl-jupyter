(asdf:defsystem #:cl-jupyter-widgets
  :description "Jupyter widgets for cl-jupyter"
  :version "1.0"
  :author "Kevin Esslinger, Alex Wood, and Christian Schafmeister"
  :license "BSD 2-Clause. See LICENSE."
  :depends-on (:cl-jupyter
               :closer-mop)
  :serial t
  :pathname "src"
  :components (
               (:file "packages")
               (:file "tools")
               (:module ikernel
                :pathname "ikernel"
                :serial t
                :components ((:file "manager")
                             (:file "comm")))
               (:module iwidgets
                :pathname "iwidgets"
                :serial t
                :components
                        ((:file "init")
                         (:file "version")
                         (:module widgets
                          :pathname "widgets"
                          :serial t
                          :components ( ;;(:file "widgets-version")
                                       (:file "interface")
                                       (:file "traitlets")
                                       (:file "trait_types")
                                       (:file "widget")
                                       (:file "valuewidget")
                                       (:file "domwidget")
                                       (:file "widget_style_7")
                                       (:file "widget_core_7")
                                       (:file "widget_description_7")
                                       (:file "widget_layout_7")
                                       (:file "widget_int_7")
                                       (:file "widget_bool_7")
                                       (:file "widget_color_7")
                                       (:file "widget_image")
                                       (:file "widget_selection_7")
                                       (:file "widget_float_7")
                                       (:file "widget_button_7")
                                       (:file "widget_string_7")
                                       (:file "widget_box_7")
                                       (:file "widget_output_7")
                                       (:file "widget_selectioncontainer_7")))))))
