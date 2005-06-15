;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/color.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 10 13:46:50 2002                          */
;*    Last change :  Wed Jan  7 11:39:58 2004 (serrano)                */
;*    Copyright   :  2002-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Tex color manager                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module skribe_color
   (import skribe_configure)
   (export (skribe-color->rgb ::obj)
	   (skribe-get-used-colors)
	   (skribe-use-color! color)))

;*---------------------------------------------------------------------*/
;*    *skribe-rgb-string* ...                                          */
;*---------------------------------------------------------------------*/
(define *skribe-rgb-string*
   "255 250 250 snow
248 248 255 ghostwhite
245 245 245 whitesmoke
220 220 220 gainsboro
255 250 240 floralwhite
253 245 230 oldlace
250 240 230 linen
250 235 215 antiquewhite
255 239 213 papayawhip
255 235 205 blanchedalmond
255 228 196 bisque
255 218 185 peachpuff
255 222 173 navajowhite
255 228 181 moccasin
255 248 220 cornsilk
255 255 240 ivory
255 250 205 lemonchiffon
255 245 238 seashell
240 255 240 honeydew
245 255 250 mintcream
240 255 255 azure
240 248 255 aliceblue
230 230 250 lavender
255 240 245 lavenderblush
255 228 225 mistyrose
255 255 255 white
0 0 0 black
47 79 79 darkslategrey
105 105 105 dimgrey
112 128 144 slategrey
119 136 153 lightslategrey
190 190 190 grey
211 211 211 lightgrey
25 25 112 midnightblue
0 0 128 navy
0 0 128 navyblue
100 149 237 cornflowerblue
72 61 139 darkslateblue
106 90 205 slateblue
123 104 238 mediumslateblue
132 112 255 lightslateblue
0 0 205 mediumblue
65 105 225 royalblue
0 0 255 blue
30 144 255 dodgerblue
0 191 255 deepskyblue
135 206 235 skyblue
135 206 250 lightskyblue
70 130 180 steelblue
176 196 222 lightsteelblue
173 216 230 lightblue
176 224 230 powderblue
175 238 238 paleturquoise
0 206 209 darkturquoise
72 209 204 mediumturquoise
64 224 208 turquoise
0 255 255 cyan
224 255 255 lightcyan
95 158 160 cadetblue
102 205 170 mediumaquamarine
127 255 212 aquamarine
0 100 0 darkgreen
85 107 47 darkolivegreen
143 188 143 darkseagreen
46 139 87 seagreen
60 179 113 mediumseagreen
32 178 170 lightseagreen
152 251 152 palegreen
0 255 127 springgreen
124 252 0 lawngreen
0 255 0 green
127 255 0 chartreuse
0 250 154 mediumspringgreen
173 255 47 greenyellow
50 205 50 limegreen
154 205 50 yellowgreen
34 139 34 forestgreen
107 142 35 olivedrab
189 183 107 darkkhaki
240 230 140 khaki
238 232 170 palegoldenrod
250 250 210 lightgoldenrodyellow
255 255 224 lightyellow
255 255 0 yellow
255 215 0 gold
238 221 130 lightgoldenrod
218 165 32 goldenrod
184 134 11 darkgoldenrod
188 143 143 rosybrown
205 92 92 indianred
139 69 19 saddlebrown
160 82 45 sienna
205 133 63 peru
222 184 135 burlywood
245 245 220 beige
245 222 179 wheat
244 164 96 sandybrown
210 180 140 tan
210 105 30 chocolate
178 34 34 firebrick
165 42 42 brown
233 150 122 darksalmon
250 128 114 salmon
255 160 122 lightsalmon
255 165 0 orange
255 140 0 darkorange
255 127 80 coral
240 128 128 lightcoral
255 99 71 tomato
255 69 0 orangered
255 0 0 red
255 105 180 hotpink
255 20 147 deeppink
255 192 203 pink
255 182 193 lightpink
219 112 147 palevioletred
176 48 96 maroon
199 21 133 mediumvioletred
208 32 144 violetred
255 0 255 magenta
238 130 238 violet
221 160 221 plum
218 112 214 orchid
186 85 211 mediumorchid
153 50 204 darkorchid
148 0 211 darkviolet
138 43 226 blueviolet
160 32 240 purple
147 112 219 mediumpurple
216 191 216 thistle
255 250 250 snow1
238 233 233 snow2
205 201 201 snow3
139 137 137 snow4
255 245 238 seashell1
238 229 222 seashell2
205 197 191 seashell3
139 134 130 seashell4
255 239 219 antiquewhite1
238 223 204 antiquewhite2
205 192 176 antiquewhite3
139 131 120 antiquewhite4
255 228 196 bisque1
238 213 183 bisque2
205 183 158 bisque3
139 125 107 bisque4
255 218 185 peachpuff1
238 203 173 peachpuff2
205 175 149 peachpuff3
139 119 101 peachpuff4
255 222 173 navajowhite1
238 207 161 navajowhite2
205 179 139 navajowhite3
139 121 94 navajowhite4
255 250 205 lemonchiffon1
238 233 191 lemonchiffon2
205 201 165 lemonchiffon3
139 137 112 lemonchiffon4
255 248 220 cornsilk1
238 232 205 cornsilk2
205 200 177 cornsilk3
139 136 120 cornsilk4
255 255 240 ivory1
238 238 224 ivory2
205 205 193 ivory3
139 139 131 ivory4
240 255 240 honeydew1
224 238 224 honeydew2
193 205 193 honeydew3
131 139 131 honeydew4
255 240 245 lavenderblush1
238 224 229 lavenderblush2
205 193 197 lavenderblush3
139 131 134 lavenderblush4
255 228 225 mistyrose1
238 213 210 mistyrose2
205 183 181 mistyrose3
139 125 123 mistyrose4
240 255 255 azure1
224 238 238 azure2
193 205 205 azure3
131 139 139 azure4
131 111 255 slateblue1
122 103 238 slateblue2
105 89 205 slateblue3
71 60 139 slateblue4
72 118 255 royalblue1
67 110 238 royalblue2
58 95 205 royalblue3
39 64 139 royalblue4
0 0 255 blue1
0 0 238 blue2
0 0 205 blue3
0 0 139 blue4
30 144 255 dodgerblue1
28 134 238 dodgerblue2
24 116 205 dodgerblue3
16 78 139 dodgerblue4
99 184 255 steelblue1
92 172 238 steelblue2
79 148 205 steelblue3
54 100 139 steelblue4
0 191 255 deepskyblue1
0 178 238 deepskyblue2
0 154 205 deepskyblue3
0 104 139 deepskyblue4
135 206 255 skyblue1
126 192 238 skyblue2
108 166 205 skyblue3
74 112 139 skyblue4
176 226 255 lightskyblue1
164 211 238 lightskyblue2
141 182 205 lightskyblue3
96 123 139 lightskyblue4
202 225 255 lightsteelblue1
188 210 238 lightsteelblue2
162 181 205 lightsteelblue3
110 123 139 lightsteelblue4
191 239 255 lightblue1
178 223 238 lightblue2
154 192 205 lightblue3
104 131 139 lightblue4
224 255 255 lightcyan1
209 238 238 lightcyan2
180 205 205 lightcyan3
122 139 139 lightcyan4
187 255 255 paleturquoise1
174 238 238 paleturquoise2
150 205 205 paleturquoise3
102 139 139 paleturquoise4
152 245 255 cadetblue1
142 229 238 cadetblue2
122 197 205 cadetblue3
83 134 139 cadetblue4
0 245 255 turquoise1
0 229 238 turquoise2
0 197 205 turquoise3
0 134 139 turquoise4
0 255 255 cyan1
0 238 238 cyan2
0 205 205 cyan3
0 139 139 cyan4
127 255 212 aquamarine1
118 238 198 aquamarine2
102 205 170 aquamarine3
69 139 116 aquamarine4
193 255 193 darkseagreen1
180 238 180 darkseagreen2
155 205 155 darkseagreen3
105 139 105 darkseagreen4
84 255 159 seagreen1
78 238 148 seagreen2
67 205 128 seagreen3
46 139 87 seagreen4
154 255 154 palegreen1
144 238 144 palegreen2
124 205 124 palegreen3
84 139 84 palegreen4
0 255 127 springgreen1
0 238 118 springgreen2
0 205 102 springgreen3
0 139 69 springgreen4
0 255 0 green1
0 238 0 green2
0 205 0 green3
0 139 0 green4
127 255 0 chartreuse1
118 238 0 chartreuse2
102 205 0 chartreuse3
69 139 0 chartreuse4
192 255 62 olivedrab1
179 238 58 olivedrab2
154 205 50 olivedrab3
105 139 34 olivedrab4
202 255 112 darkolivegreen1
188 238 104 darkolivegreen2
162 205 90 darkolivegreen3
110 139 61 darkolivegreen4
255 246 143 khaki1
238 230 133 khaki2
205 198 115 khaki3
139 134 78 khaki4
255 236 139 lightgoldenrod1
238 220 130 lightgoldenrod2
205 190 112 lightgoldenrod3
139 129 76 lightgoldenrod4
255 255 224 lightyellow1
238 238 209 lightyellow2
205 205 180 lightyellow3
139 139 122 lightyellow4
255 255 0 yellow1
238 238 0 yellow2
205 205 0 yellow3
139 139 0 yellow4
255 215 0 gold1
238 201 0 gold2
205 173 0 gold3
139 117 0 gold4
255 193 37 goldenrod1
238 180 34 goldenrod2
205 155 29 goldenrod3
139 105 20 goldenrod4
255 185 15 darkgoldenrod1
238 173 14 darkgoldenrod2
205 149 12 darkgoldenrod3
139 101 8 darkgoldenrod4
255 193 193 rosybrown1
238 180 180 rosybrown2
205 155 155 rosybrown3
139 105 105 rosybrown4
255 106 106 indianred1
238 99 99 indianred2
205 85 85 indianred3
139 58 58 indianred4
255 130 71 sienna1
238 121 66 sienna2
205 104 57 sienna3
139 71 38 sienna4
255 211 155 burlywood1
238 197 145 burlywood2
205 170 125 burlywood3
139 115 85 burlywood4
255 231 186 wheat1
238 216 174 wheat2
205 186 150 wheat3
139 126 102 wheat4
255 165 79 tan1
238 154 73 tan2
205 133 63 tan3
139 90 43 tan4
255 127 36 chocolate1
238 118 33 chocolate2
205 102 29 chocolate3
139 69 19 chocolate4
255 48 48 firebrick1
238 44 44 firebrick2
205 38 38 firebrick3
139 26 26 firebrick4
255 64 64 brown1
238 59 59 brown2
205 51 51 brown3
139 35 35 brown4
255 140 105 salmon1
238 130 98 salmon2
205 112 84 salmon3
139 76 57 salmon4
255 160 122 lightsalmon1
238 149 114 lightsalmon2
205 129 98 lightsalmon3
139 87 66 lightsalmon4
255 165 0 orange1
238 154 0 orange2
205 133 0 orange3
139 90 0 orange4
255 127 0 darkorange1
238 118 0 darkorange2
205 102 0 darkorange3
139 69 0 darkorange4
255 114 86 coral1
238 106 80 coral2
205 91 69 coral3
139 62 47 coral4
255 99 71 tomato1
238 92 66 tomato2
205 79 57 tomato3
139 54 38 tomato4
255 69 0 orangered1
238 64 0 orangered2
205 55 0 orangered3
139 37 0 orangered4
255 0 0 red1
238 0 0 red2
205 0 0 red3
139 0 0 red4
255 20 147 deeppink1
238 18 137 deeppink2
205 16 118 deeppink3
139 10 80 deeppink4
255 110 180 hotpink1
238 106 167 hotpink2
205 96 144 hotpink3
139 58 98 hotpink4
255 181 197 pink1
238 169 184 pink2
205 145 158 pink3
139 99 108 pink4
255 174 185 lightpink1
238 162 173 lightpink2
205 140 149 lightpink3
139 95 101 lightpink4
255 130 171 palevioletred1
238 121 159 palevioletred2
205 104 137 palevioletred3
139 71 93 palevioletred4
255 52 179 maroon1
238 48 167 maroon2
205 41 144 maroon3
139 28 98 maroon4
255 62 150 violetred1
238 58 140 violetred2
205 50 120 violetred3
139 34 82 violetred4
255 0 255 magenta1
238 0 238 magenta2
205 0 205 magenta3
139 0 139 magenta4
255 131 250 orchid1
238 122 233 orchid2
205 105 201 orchid3
139 71 137 orchid4
255 187 255 plum1
238 174 238 plum2
205 150 205 plum3
139 102 139 plum4
224 102 255 mediumorchid1
209 95 238 mediumorchid2
180 82 205 mediumorchid3
122 55 139 mediumorchid4
191 62 255 darkorchid1
178 58 238 darkorchid2
154 50 205 darkorchid3
104 34 139 darkorchid4
155 48 255 purple1
145 44 238 purple2
125 38 205 purple3
85 26 139 purple4
171 130 255 mediumpurple1
159 121 238 mediumpurple2
137 104 205 mediumpurple3
93 71 139 mediumpurple4
255 225 255 thistle1
238 210 238 thistle2
205 181 205 thistle3
139 123 139 thistle4
0 0 0 grey0
3 3 3 grey1
5 5 5 grey2
8 8 8 grey3
10 10 10 grey4
13 13 13 grey5
15 15 15 grey6
18 18 18 grey7
20 20 20 grey8
23 23 23 grey9
26 26 26 grey10
28 28 28 grey11
31 31 31 grey12
33 33 33 grey13
36 36 36 grey14
38 38 38 grey15
41 41 41 grey16
43 43 43 grey17
46 46 46 grey18
48 48 48 grey19
51 51 51 grey20
54 54 54 grey21
56 56 56 grey22
59 59 59 grey23
61 61 61 grey24
64 64 64 grey25
66 66 66 grey26
69 69 69 grey27
71 71 71 grey28
74 74 74 grey29
77 77 77 grey30
79 79 79 grey31
82 82 82 grey32
84 84 84 grey33
87 87 87 grey34
89 89 89 grey35
92 92 92 grey36
94 94 94 grey37
97 97 97 grey38
99 99 99 grey39
102 102 102 grey40
105 105 105 grey41
107 107 107 grey42
110 110 110 grey43
112 112 112 grey44
115 115 115 grey45
117 117 117 grey46
120 120 120 grey47
122 122 122 grey48
125 125 125 grey49
127 127 127 grey50
130 130 130 grey51
133 133 133 grey52
135 135 135 grey53
138 138 138 grey54
140 140 140 grey55
143 143 143 grey56
145 145 145 grey57
148 148 148 grey58
150 150 150 grey59
153 153 153 grey60
156 156 156 grey61
158 158 158 grey62
161 161 161 grey63
163 163 163 grey64
166 166 166 grey65
168 168 168 grey66
171 171 171 grey67
173 173 173 grey68
176 176 176 grey69
179 179 179 grey70
181 181 181 grey71
184 184 184 grey72
186 186 186 grey73
189 189 189 grey74
191 191 191 grey75
194 194 194 grey76
196 196 196 grey77
199 199 199 grey78
201 201 201 grey79
204 204 204 grey80
207 207 207 grey81
209 209 209 grey82
212 212 212 grey83
214 214 214 grey84
217 217 217 grey85
219 219 219 grey86
222 222 222 grey87
224 224 224 grey88
227 227 227 grey89
229 229 229 grey90
232 232 232 grey91
235 235 235 grey92
237 237 237 grey93
240 240 240 grey94
242 242 242 grey95
245 245 245 grey96
247 247 247 grey97
250 250 250 grey98
252 252 252 grey99
255 255 255 grey100
169 169 169 darkgrey
0 0 139 darkblue
0 139 139 darkcyan
139 0 139 darkmagenta
139 0 0 darkred
144 238 144 lightgreen")

;*---------------------------------------------------------------------*/
;*    *rgb-port* ...                                                   */
;*---------------------------------------------------------------------*/
(define *rgb-port* #unspecified)

;*---------------------------------------------------------------------*/
;*    same-color? ...                                                  */
;*---------------------------------------------------------------------*/
(define (same-color? s1 s2)
   (define (skip-rgb s)
      (let ((l (string-length s)))
	 (let loop ((i 0))
	    (if (=fx i l)
		l
		(let ((c (string-ref s i)))
		   (if (or (char-numeric? c) (char-whitespace? c))
		       (loop (+fx i 1))
		       i))))))
   (let ((l1 (string-length s1))
	 (l2 (string-length s2)))
      (if (>fx l1 l2)
	  (let ((lc (skip-rgb s1)))
	     (and (=fx (-fx l1 lc) l2)
		  (let loop ((i1 (-fx l1 l2))
			     (i2 0))
		     (cond
			((=fx i1 l1)
			 #t)
			((char-ci=? (string-ref s1 i1) (string-ref s2 i2))
			 (loop (+fx i1 1) (+fx i2 1)))
			(else
			 #f))))))))

;*---------------------------------------------------------------------*/
;*    rgb-grep ...                                                     */
;*---------------------------------------------------------------------*/
(define (rgb-grep symbol)
   (let ((parser (regular-grammar ()
		    ((bol (: #\! (* all)))
		     (ignore))
		    ((+ #\Newline)
		     (ignore))
		    ((: (* (in #\space #\tab))
			(+ digit)
			(+ (in #\space #\tab))
			(+ digit)
			(+ (in #\space #\tab))
			(+ digit)
			(+ (in #\space #\tab))
			(+ all))
		     (let ((s (the-string)))
			(if (same-color? s symbol)
			    (let ((m (pregexp-match "[ \t]*([0-9]+)[ \t]+([0-9]+)[ \t]+([0-9]+)[ \t]+.+" s)))
			       (values (string->number (cadr m))
				       (string->number (caddr m))
				       (string->number (cadddr m))))
			    (ignore))))
		    (else
		     (values 0 0 0)))))
      ;; initialization the port reading rgb.txt file
      (with-input-from-string *skribe-rgb-string*
	 (lambda ()
	    (read/rp parser (current-input-port))))))

;*---------------------------------------------------------------------*/
;*    *color-parser* ...                                               */
;*---------------------------------------------------------------------*/
(define *color-parser*
   (regular-grammar ((blank* (* blank))
		     (blank+ (+ blank)))
      
      ;; rgb color
      ((: #\# (+ xdigit))
       (let ((val (the-substring 1 (the-length))))
	  (cond
	     ((=fx (string-length val) 6)
	      (values (string->integer (substring val 0 2) 16)
		      (string->integer (substring val 2 4) 16)
		      (string->integer (substring val 4 6) 16)))
	     ((=fx (string-length val) 12)
	      (values (string->integer (substring val 0 2) 16)
		      (string->integer (substring val 4 6) 16)
		      (string->integer (substring val 8 10) 16)))
	     (else
	      (values 0 0 0)))))
      
      ;; symbolic names
      ((+ (out #\Newline))
       (let ((name (the-string)))
	  (cond
	     ((string-ci=? name "none")
	      (values 0 0 0))
	     ((string-ci=? name "black")
	      (values #xff #xff #xff))
	     ((string-ci=? name "white")
	      (values 0 0 0))
	     (else
	      (rgb-grep name)))))
      
      ;; error
      (else
       (values 0 0 0))))

;*---------------------------------------------------------------------*/
;*    skribe-color->rgb ...                                            */
;*---------------------------------------------------------------------*/
(define (skribe-color->rgb spec)
   (cond
      ((string? spec)
       (with-input-from-string spec
	  (lambda ()
	     (read/rp *color-parser* (current-input-port)))))
      ((fixnum? spec)
       (values (bit-and #xff (bit-rsh spec 16))
	       (bit-and #xff (bit-rsh spec 8))
	       (bit-and #xff spec)))
      (else
       (values 0 0 0))))

;*---------------------------------------------------------------------*/
;*    *used-colors* ...                                                */
;*---------------------------------------------------------------------*/
(define *used-colors* '())

;*---------------------------------------------------------------------*/
;*    skribe-get-used-colors ...                                       */
;*---------------------------------------------------------------------*/
(define (skribe-get-used-colors)
   *used-colors*)

;*---------------------------------------------------------------------*/
;*    skribe-use-color! ...                                            */
;*---------------------------------------------------------------------*/
(define (skribe-use-color! color)
   (set! *used-colors* (cons color *used-colors*))
   color)
