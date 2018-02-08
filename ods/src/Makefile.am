all: build.ninja
	ninja

build.ninja: build.yaml
	ng -f build.yaml $@

clean:
	ninja -t clean
