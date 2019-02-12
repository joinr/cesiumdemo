;;Provides a wrapper around cesium's
;;baked in colors.
(ns cesiumdemo.colors)

(def cesium-colors
  {:alice-blue "ALICEBLUE"
   :antique-white "ANTIQUEWHITE"
   :aqua "AQUA"
   :aqua-marine "AQUAMARINE"
   :azure "AZURE"
   :beiege "BEIGE"
   :bisque "BISQUE"
   :black "BLACK"
   :blanched-almond "BLANCHEDALMOND"
   :blue "BLUE"
   :blue-violet "BLUEVIOLET"
   :brown "BROWN"
   :burly-wood "BURLYWOOD"
   :cadet-blue "CADETBLUE"
   :chartreuse "CHARTREUSE"
   :chocolate "CHOCOLATE"
   :coral "CORAL"
   :corn-flower-blue "CORNFLOWERBLUE"
   :corn-silk "CORNSILK"
   :crimson "CRIMSON"
   :cyan "CYAN"
   :dark-blue "DARKBLUE"
   :dark-cyan "DARKCYAN"
   :dark-golden-rod "DARKGOLDENROD"
   :dark-gray "DARKGRAY"
   :dark-green "DARKGREEN"
   :dark-grey "DARKGREY"
   :dark-khaki "DARKKHAKI"
   :dark-magenta "DARKMAGENTA"
   :dark-olive-green "DARKOLIVEGREEN"
   :dark-orange "DARKORANGE"
   :dark-orchid "DARKORCHID"
   :dark-red "DARKRED"
   :dark-salmon "DARKSALMON"
   :dark-sea-green "DARKSEAGREEN"
   :dark-slate-blue "DARKSLATEBLUE"
   :dark-slate-gray "DARKSLATEGRAY"
   :dark-slate-grey "DARKSLATEGREY"
   :dark-turquoise "DARKTURQUOISE"
   :dark-violet "DARKVIOLET"
   :deep-pink "DEEPPINK"
   :deep-sky-blue "DEEPSKYBLUE"
   :dim-gray "DIMGRAY"
   :dim-grey "DIMGREY"
   :dodger-blue "DODGERBLUE"
   :fire-brick "FIREBRICK"
   :floral-white "FLORALWHITE"
   :forest-green "FORESTGREEN"
   :fuchsia "FUCHSIA"
   :gainsboro "GAINSBORO"
   :ghost-white "GHOSTWHITE"
   :gold "GOLD"
   :golden-rod"GOLDENROD"
   :gray "GRAY"
   :green "GREEN"
   :green-yellow "GREENYELLOW"
   :grey "GREY"
   :honey-dew "HONEYDEW"
   :hot-pink "HOTPINK"
   :indian-red "INDIANRED"
   :indigo "INDIGO"
   :ivory "IVORY"
   :khaki "KHAKI"
   :lavender "LAVENDER"
   :lavendar-blush "LAVENDAR_BLUSH"
   :lawn-green "LAWNGREEN"
   :lemon-chiffon "LEMONCHIFFON"
   :light-blie "LIGHTBLUE"
   :light-coral "LIGHTCORAL"
   :light-cyan "LIGHTCYAN"
   :light-golden-rod-yellow "LIGHTGOLDENRODYELLOW"
   :light-gray "LIGHTGRAY"
   :light-green "LIGHTGREEN"
   :light-grey "LIGHTGREY"
   :light-pink "LIGHTPINK"
   :light-sea-green "LIGHTSEAGREEN"
   :light-sky-blue "LIGHTSKYBLUE"
   :light-slate-gray "LIGHTSLATEGRAY"
   :light-slate-grey "LIGHTSLATEGREY"
   :light-steel-blue "LIGHTSTEELBLUE"
   :light-yellow "LIGHTYELLOW"
   :lime "LIME"
   :lime-green "LIMEGREEN"
   :linen "LINEN"
   :magenta "MAGENTA"
   :maroon "MAROON"
   :medium-aqua-marine "MEDIUMAQUAMARINE"
   :medium-blue "MEDIUMBLUE"
   :medium-orchid "MEDIUMORCHID"
   :medium-purple "MEDIUMPURPLE"
   :medium-sea-green "MEDIUMSEAGREEN"
   :medium-slate-blue "MEDIUMSLATEBLUE"
   :medium-spring-green "MEDIUMSPRINGGREEN"
   :medium-turquoise "MEDIUMTURQUOISE"
   :medium-violet-red "MEDIUMVIOLETRED"
   :midnight-blue "MIDNIGHTBLUE"
   :mint-cream "MINTCREAM"
   :misty-rose "MISTYROSE"
   :moccasin "MOCCASIN"
   :navajo-white "NAVAJOWHITE"
   :navy "NAVY"
   :old-place "OLDLACE"
   :olive "OLIVE"
   :olive-drab "OLIVEDRAB"
   :orange "ORANGE"
   :orange-red "ORANGERED"
   :orchid "ORCHID"
   :pale-golden-rod "PALEGOLDENROD"
   :pale-green "PALEGREEN"
   :pale-turquoise "PALETURQUOISE"
   :pale-violet-red "PALEVIOLETRED"
   :papaya-whip "PAPAYAWHIP"
   :peach-puff "PEACHPUFF"
   :peru "PERU"
   :pink "PINK"
   :plum "PLUM"
   :powder-blue "POWDERBLUE"
   :purple "PURPLE"
   :red "RED"
   :rosy-brown "ROSYBROWN"
   :royal-blue "ROYALBLUE"
   :saddle-brown "SADDLEBROWN"
   :salmon "SALMON"
   :sandy-brown "SANDYBROWN"
   :sea-green "SEAGREEN"
   :sea-shell "SEASHELL"
   :sienna "SIENNA"
   :silver "SILVER"
   :sky-blue "SKYBLUE"
   :slate-blue "SLATEBLUE"
   :slate-gray "SLATEGRAY"
   :slate-grey "SLATEGREY"
   :snow "SNOW"
   :spring-green "SPRINGGREEN"
   :steel-blue "STEELBLUE"
   :tan "TAN"
   :teal "TEAL"
   :thistle "THISTLE"
   :tomato "TOMATO"
   :turquoise "TURQUOISE"
   :violet "VIOLET"
   :wheat "WHEAT"
   :white "WHITE"
   :white-smoke "WHITESMOKE"
   :yellow "YELLOW"
   :yellow-green "YELLOWGREEN"
   :transparent "TRANSPARENT"})

(defn byte->float [f]
  (Cesium.Color.byteToFloat f))
(defn float->byte [f]
  (Cesium.Color.floatToByte f))

(extend-protocol color/IColorVector
  Cesium.Color
  (color-vec [c] 
    [(float->byte (.-red c))
     (float->byte (.-green c)) 
     (float->byte (.-blue c)) 
     (float->byte (.-alpha c))]))

;;cesium color space is [0.0 1.0]
;;versus the cljcolor [0 255]
;;so if we'd like to use the palletes
;;from cljcolor, we need to convert.
(defprotocol ICesiumColor
  (-as-color [c]))


(defn ->color [v] 
  (Cesium.Color. 
      (color/get-r c)
      (color/get-g c)
      (color/get-b c)
      (color/get-a c)))

(defn bytes->color [v]
  (Cesium.Color.fromBytes 
    (color/get-r c)
    (color/get-g c)
    (color/get-b c)
    (color/get-a c)))

;;allows us to unify with cljcolor palette
(defn palette-color->color [c]
  (when-let [spec (get cljcolor.core/*pallette* c)]
    (bytes->color spec)))

(extend-protocol ICesiumColor
  cljs.core.PersistentVector
  (-as-color [c] (->color c))  
  cljs.core.Keyword
  (-as-color [c]    
    (-as-color (or (get cesium-colors c) ;;default cesium colors
                   (palette-color->color c)
                   (name c))))
  string 
  (-as-color [c]
    (Cesium.Color.fromCssColorString c)))

