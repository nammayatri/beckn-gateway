IMAGE_NAME ?= beckn-gateway

SOURCE_COMMIT := $(shell git rev-parse HEAD)
VERSION := $(shell git rev-parse --short HEAD)

DEP_IMAGE ?= beckn-gateway-dep

.PHONY: build-dep build run-svc stop-all-containers run run-pgadmin pre-commit

run-svc: ./dev/docker-compose.yml
	# Setup and run DB, redis and passetto instances in docker containers
	docker-compose -f ./dev/docker-compose.yml up -d --remove-orphans

stop-all-containers: ./dev/docker-compose.yml
	# Stop all docker containers
	docker-compose -f ./dev/docker-compose.yml down --remove-orphans

run: ./dev/run.sh
	# Run all binaries
	./dev/run.sh

run-pgadmin: ./dev/docker-compose.yml
	# Run pgadmin stack - Pgadmin in a docker container
	docker-compose -f ./dev/docker-compose.yml --profile pgadmin up -d

build-dep: Dockerfile.dep
	$(info Building $(DEP_IMAGE):latest / git-head: $(SOURCE_COMMIT))
	docker build -t $(DEP_IMAGE):latest -f Dockerfile.dep .

build: Dockerfile
	$(info Building $(IMAGE_NAME):$(VERSION) / git-head: $(SOURCE_COMMIT))
	docker build -t $(IMAGE_NAME):$(VERSION) -f Dockerfile . --build-arg "DEP_IMAGE_PATH=$(DEP_IMAGE)"

pre-commit: ./dev/format-all-files.sh
	#Checking and formatting .hs and .dhall files
	./dev/format-all-files.sh