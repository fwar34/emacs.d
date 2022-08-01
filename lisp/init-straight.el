(defhydra hydra-straight-helper (:hint nil)
    "
_c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all
_C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package
----------------^^+--------------^^+---------------^^+----------------^^+------------||_q_uit||
_r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe
_R_ebuild package |_P_ull package  |_V_ersions thaw  |                  |prun_e_ build"
    ("c" straight-check-all)
    ("C" straight-check-package)
    ("r" straight-rebuild-all)
    ("R" straight-rebuild-package)
    ("f" straight-fetch-all)
    ("F" straight-fetch-package)
    ("p" straight-pull-all)
    ("P" straight-pull-package)
    ("m" straight-merge-all)
    ("M" straight-merge-package)
    ("n" straight-normalize-all)
    ("N" straight-normalize-package)
    ("u" straight-push-all)
    ("U" straight-push-package)
    ("v" straight-freeze-versions)
    ("V" straight-thaw-versions)
    ("w" straight-watcher-start)
    ("g" straight-get-recipe)
    ("e" straight-prune-build)
    ("q" nil))

(transient-define-prefix my-straight-transient ()
    " <straight commands>"
    ["                                                          <straight commands>"
     [("c" "check all" straight-check-all)
      ("C" "straight-check-package" straight-check-package)
      ("r" "straight-rebuild-all" straight-rebuild-all)
      ("R" "straight-rebuild-package" straight-rebuild-package)]
     [("f" "straight-fetch-all" straight-fetch-all)
      ("F" "straight-fetch-package" straight-fetch-package)
      ("p" "straight-pull-all" straight-pull-all)
      ("P" "straight-pull-package" straight-pull-package)]
     [("m" "straight-merge-all" straight-merge-all)
      ("M" "straight-merge-package" straight-merge-package)
      ("n" "straight-normalize-all" straight-normalize-all)
      ("N" "straight-normalize-package" straight-normalize-package)]
     [("u" "straight-push-all" straight-push-all)
      ("U" "straight-push-package" straight-push-package)
      ("v" "straight-freeze-versions" straight-freeze-versions)
      ("V" "straight-thaw-versions" straight-thaw-versions)]
     [("w" "straight-watcher-start" straight-watcher-start)
      ("W" "straight-watcher-stop" straight-watcher-stop)
      ("g" "straight-get-recipe" straight-get-recipe)
      ("e" "straight-prune-build" straight-prune-build)]])
