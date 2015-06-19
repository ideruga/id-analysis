heroku build -r -b https://github.com/begriffs/heroku-buildpack-ghc.git
heroku addons:add heroku-postgresql:dev
git remote rm heroku
git remote add heroku git@heroku.com:tranquil-earth-3510.git
git push heroku master
