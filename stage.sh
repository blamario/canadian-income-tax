cabal build
rsync -av `cabal list-bin serve-canadian-taxes` ../hosted/taxell/
rsync -av data                                  ../hosted/taxell/
rsync -av web/client/build                      ../hosted/taxell/web/client/
rsync -av web/client/public                     ../hosted/taxell/web/client/
rsync -av web/static/about.html                 ../hosted/taxell/web/static/
strip ../hosted/taxell/serve-canadian-taxes
