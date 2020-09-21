
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Houdini VEX Mode
;; by Jens Beier  jbeier@gmx.net
;;
;; originally based on RSL mode by 
;; steve may, accad, 4/27/95
;; (see http://www.accad.ohio-state.edu/~smay/RManNotes/rsl_emacs_mode.html)
;;----------------------------------------
;;
;; To make this mode enabled whenever a file ending in ".vfl" is loaded,
;; put the vfl-mode.el file into an emacs autoload dir
;; and add the following to your .emacs file.
;;
;; (require 'vfl-mode)
;;
;; to activate syntax highlighting, add the following to your .emacs file:
;; (global-font-lock-mode 1)
;;----------------------------------------
;;

(require 'font-lock)
(require 'cc-mode)


(defvar vfl-mode-hook nil)



(defun vfl-mode ()
  "Major mode for editing RenderMan shaders.
This is actually just C mode with commands for compiling and 
rendering shaders.

C-c C-c    save buffer & compile shader
C-c C-i    set the current include directories as LISP list of strings;
           each string denoting one directory. For example (at the prompt):
           (\"/usr/local/shaders\" \"/usr/shaders\").

To get info on C mode, select 'Describe Function...' from the 'Help'
menu and enter 'c-mode' at the prompt.
"
  (interactive)
  (c-mode)
  (local-set-key "\C-c\C-c" 'vfl-compile)
  (local-set-key "\C-c\C-i" 'vfl-set-inc-dirs)
  (setq major-mode 'vfl-mode)
  (setq mode-name "Houdini VEX")
  (make-variable-buffer-local 'compilation-error-regexp-alist)
  (setq compilation-error-regexp-alist 
        (append compilation-error-regexp-alist 
                vcc-compilation-regexp-alist))
  (setq c-conditional-key "\\<\\(for\\|if\\|do\\|else\\|while\\|switch\\|forpoints\\)\\>[^_]")
  (setq c-font-lock-extra-types
        (append '("vector" "vector4" "matrix3" "matrix" "string")
                c-font-lock-extra-types))
  (setq font-lock-defaults '(vfl-font-lock-keywords nil nil ((?_ . "w"))))
  (run-hooks 'vfl-mode-hook))




;; vcc error message
;;"WoodPlanks.vfl" line 73 ERROR (1010) Undefined instruction "nax"
(setq vcc-compilation-regexp-alist
      '(("\"\\(.+\\)\" line \\([0-9]+\\) .+" 1 2)))




(defvar vfl-font-lock-keywords-3
    '(("\\<\\(Cf\\|Of\\|Af\\|P\\|Pz\\|Ps\\|I\\|Eye\\|s\\|t\\|dPds\\|dPdt\\|N\\|Ng\\|Cl\\|L\\|Lz\\)\\>" . font-lock-constant-face)
      ("\\<\\(XRES\\|YRES\\|AR\\|IX\\|IY\\|X\\|Y\\|H\\|S\\|V\\|R\\|G\\|B\\|A\\|C1\\|C2\\|C3\\|C4\\|PNAME\\|PL\\|PS\\|AI\\|AS\\|NP\\|NI\\|F\\|SF\\|EF\\|TIME\\|TINC\\|FR\\)\\>" . font-lock-constant-face)
      ("\\<\\(ptnum\\|Npt\\|Frame\\|Time\\|TimeInc\\|v\\|accel\\|Cd\\|id\\|age\\|life\\|pstate\\)\\>" . font-lock-constant-face)
      ("\\<\\(E\\|SR\\|L\\|C\\|NC\\)\\>" . font-lock-constant-face)
      ("\\<\\(density\\)\\>" . font-lock-constant-face)
      ("\\<\\(Du\\|Dv\\|area\\|filterstep\\|computenormal\\|getobjectname\\|trace\\|rayhittest\\|fastshadow\\|filtershadow\\|getraylevel\\|getrayweight\\|getglobalraylevel\\|texture\\|environment\\|otransform\\|ltransform\\|ftransform\\|ontransform\\|lntransform\\|fntransform\\|ovtransform\\|lvtransform\\|fvtransform\\)\\>" . font-lock-builtin-face)
      ("\\<\\(shadow\\|ambient\\|irradiance\\|occlusion\\|diffuse\\|getlightname\\|phong\\|blinn\\|specular\\|reflectlight\\|refractlight\\|isshadowray\\|dimport\\|limport\\)\\>" . font-lock-builtin-face)
      ("\\<\\(photon_switch\\|photon_diffuse\\|photon_reflect\\|photon_transmit\\|photon_store\\)\\>" . font-lock-builtin-face)
      ("\\<\\(simport\\)\\>" . font-lock-builtin-face)
      ("\\<\\(__nondiffuse\\|__nonspecular\\|__nofog\\)\\>" . font-lock-builtin-face)
      ("\\<\\(assign\\|set\\|getcomp\\|setcomp\\|abs\\|acos\\|asin\\|atan\\|ceil\\|cos\\|cosh\\|degrees\\|exp\\|floor\\|frac\\|log\\|log10\\|pow\\|radians\\|rint\\|sin\\|sinh\\|sqrt\\|tan\\|tanh\\)\\>" . font-lock-builtin-face)
      ("\\<\\(distance\\|distance2\\|dot\\|length\\|length2\\|normalize\\|avg\\|max\\|min\\|cross\\)\\>" . font-lock-builtin-face)
      ("\\<\\(clamp\\|fit\\|lerp\\|smooth\\|cspline\\|ckspline\\|lkspline\\)\\>" . font-lock-builtin-face)
      ("\\<\\(determinant\\|dihedral\\|ident\\|invert\\|lookat\\|rotate\\|scale\\|translate\\|transpose\\|maketransform\\|vtransform\\|cracktransform\\)\\>" . font-lock-builtin-face)
      ("\\<\\(clip\\|ptlined\\)\\>" . font-lock-builtin-face)
      ("\\<\\(atten\\|fresnel\\|frontface\\|reflect\\|refract\\)\\>" . font-lock-builtin-face)
      ("\\<\\(hsvtorgb\\|luminance\\|rgbtohsv\\)\\>" . font-lock-builtin-face)
      ("\\<\\(concat\\|strlen\\|atoi\\|match\\)\\>" . font-lock-builtin-face)
      ("\\<\\(noise\\|pnoise\\|wnoise\\|vnoise\\|onoise\\|snoise\\|anoise\\|hscript_noise\\|hscript_turb\\|hscript_snoise\\|hscript_sturb\\)\\>" . font-lock-builtin-face)
      ("\\<\\(random\\|hscript_rand\\|nrandom\\)\\>" . font-lock-builtin-face)
      ("\\<\\(colormap\\|rawcolormap\\|bumpmap\\|bumpmapR\\|bumpmapG\\|bumpmapB\\|bumpmapA\\|bumpmapL\\|rawbumpmap\\|rawbumpmapR\\|rawbumpmapG\\|rawbumpmapB\\|rawbumpmapA\\|rawbumpmapL\\|depthmap\\|shadowmap\\)\\>" . font-lock-builtin-face)
      ("\\<\\(photonmap\\|texture3d\\|gradient3d\\|intersect3d\\|integrate3d\\|texture3dBox\\|integrate3dClip\\)\\>" . font-lock-builtin-face)
      ("\\<\\(getbounds\\|npoints\\|nprimitives\\|intersect\\|metaweight\\|metastart\\|metanext\\|metaimport\\|prim_normal\\|prim_attribute\\)\\>" . font-lock-builtin-face)
      ("\\<\\(ow_space\\|wo_space\\|tw_space\\|wt_space\\|ow_nspace\\|wo_nspace\\|tw_nspace\\|wt_nspace\\|ow_vspace\\|wo_vspace\\|tw_vspace\\|wt_vspace\\|toNDC\\|fromNDC\\)\\>" . font-lock-builtin-face)
      ("\\<\\(printf\\|sprintf\\|sleep\\|isbound\\)\\>" . font-lock-builtin-face)
      ("\\<\\(ch\\|optransform\\)\\>" . font-lock-builtin-face)
      ("\\<\\(hasplane\\|planeindex\\|chname\\|planesize\\|colorname\\|alphaname\\|maskname\\|depthname\\|lumname\\|bumpname\\|pointname\\|normalname\\|velocityname\\|isconnected\\|inumplanes\\|ihasplane\\|iplanename\\|ichname\\|iplaneindex\\|iplanesize\\|irate\\|istart\\|iend\\|istarttime\\|iendtime\\|ixrex\\|iyres\\|iaspect\\|cinput\\|finput\\)\\>" . font-lock-builtin-face)
      ("\\<\\(addattribute\\|addvariablename\\|newgroup\\|addgroup\\|ingroup\\|getbbox\\|relbbox\\|npoints\\|getneighbourcount\\|getneighbour\\|import\\)\\>" . font-lock-builtin-face)
      ("\\<\\(chinput\\|chstart\\|chend\\|chstartt\\|chstartf\\|chendt\\|chendf\\|chrate\\|chnumchan\\|isframes\\|isseconds\\|issamples\\)\\>" . font-lock-builtin-face)
      ("\\<\\(mattrib\\|mspace\\|mdensity\\)\\>" . font-lock-builtin-face)
      ("\\<\\(forpoints\\)\\>" . font-lock-keyword-face))
"Default expressions to highlight in vfl mode.")


(setq vfl-font-lock-keywords
  (append vfl-font-lock-keywords-3
          c-font-lock-keywords-2))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save & compile current buffer using vcc
;; 
(defvar vfl-mode-inc-list nil "List of shader compiler include directories.")


(defun vfl-exp-inc-list (alist)
  (cond
   ((null alist) "")
   (t (concat " -I" (expand-file-name (car alist)) 
	      (vfl-exp-inc-list (cdr alist))))))

(defun vfl-compile (args)
  "Save the  shader in the current buffer and compile it with vcc."
  (interactive "P")
  (save-buffer)
  (let* ((vfl-source-file (buffer-name)))
    (compile (concat "vcc" (vfl-exp-inc-list vfl-mode-inc-list) 
		     " " vfl-source-file)))
  (message "Type C-x ` to go to the next error or C-x 1 to remove compilation window."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set include dirs
;;
(defun vfl-set-inc-dirs (dirs)
  "Set the current include directories as a LISP list of strings - each string denoting one directory."
  (interactive "xList of directories to include: ")
  (setq vfl-mode-inc-list dirs))







(setq auto-mode-alist (append '(("\\.vfl$" . vfl-mode)) auto-mode-alist))

(provide 'vfl-mode)