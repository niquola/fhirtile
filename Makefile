.EXPORT_ALL_VARIABLES:

APP_IMAGE  = "us.gcr.io/aidbox2-205511/adibx-crm:latest"
CLUSTER = cluster-production
BUILT_AT = $(shell date +%FT%T)

repl:
	clj -A:test:nrepl -e "(-main)" -r 

# export GRAALVM_HOME=$HOME/graalvm/Contents/Home
# clojure -A:native-image --graalvm-opt 'H:ReflectionConfigurationFiles=reflection.json'
build-native:
	clojure -A:native-image --graalvm-opt '-enable-url-protocols=http' --graalvm-opt '-enable-url-protocols=https'

build:
	clojure -A:build

docker: build
	docker build -t ${APP_IMAGE} .

push: docker
	docker push ${APP_IMAGE}

deploy:
	cat deploy.tpl.yaml | envtpl  | kubectl apply -f -
