
(defpackage #:cl-jupyter-widgets
  (:nicknames #:cljw)
  (:use #:cl)
  (:shadow #:open #:close #:step #:min #:max)
  (:export
   #:*send-updates*
   #:ipython-display
   #:widget
   #:widget-open
   #:widget-send
   #:widget-close
   #:domwidget
   #:int-slider
   #:image
   #:bool
   #:dict
   #:unicode
   #:cunicode
   #:tuple
   #:color
   #:instance
   #:on-msg
   #:on-displayed
   ;;;For particular widgets
   #:value
   #:description
   ;;;from widget_bool_7
   #:checkbox
   #:toggle-button
   #:valid
   ;;;from widget_int_7
   #:int-text
   #:bounded-int-text
   #:int-slider
   #:int-progress
   #:int-range-slider
   ;;;from widget_float_7
   #:float-text
   #:bounded-float-text
   #:float-slider
   #:float-progress
   #:float-range-slider
   ;;;from widget_selection_7
   #:toggle-buttons
   #:dropdown
   #:radio-buttons
   #:select
   #:select-multiple
   #:selection-slider
   #:selection-range-slider
   ;;;from widget_selectioncontainer_7
   #:accordion
   #:tab
   ;;;from widget_button_7
   #:button
   ;;;from widget_box_7
   #:box
   #:vbox
   #:hbox
   ;;;from widget_color_7
   #:color-picker
   ;;;from widget_string_7
   #:html
   #:html-math
   #:label
   #:textarea
   #:text
   #:password
   )
  (:import-from :fredokun-utilities #:[] #:[]-contains))

(defpackage #:traitlets
  (:use #:cl)
  (:export #:traitlet-class #:synced-object)
  (:export #:traitlet-metadata
	   #:effective-traitlet))
