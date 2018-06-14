stage-docker:
	rm -Rf .docker-stage
	mkdir .docker-stage
	cp -r matter-language .docker-stage/
	cp -r matter-service .docker-stage/
	cp -r matter-server .docker-stage/

build-server: stage-docker
	docker build -t matter-server:app .

run-server: build-server
	docker run -i -t --network=host --rm matter-server:app