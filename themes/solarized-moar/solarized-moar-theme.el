(require 'solarized-moar-definitions
         (locate-file "solarized-moar-definitions.el" custom-theme-load-path
                      '("c" "")))

(create-solarized-theme solarized-moar
                        solarized-description
                        (solarized-moar-color-definitions))
