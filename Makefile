.EXPORT_ALL_VARIABLES:

APP_IMAGE  = "aidbox/fhir-test-runner"
BUILT_AT = $(shell date +%FT%T)

repl:
	clj -A:test:nrepl -e "(-main)" -r 

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

run:
	java -jar ./target/fhir-ts-1.0.0-standalone.jar $(case-file)

run-dir:
	ls -d $(dir)/* | xargs java -jar ./target/fhir-ts-1.0.0-standalone.jar
