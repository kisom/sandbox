pkgdata_DATA = corpus.txt ods-cpp/main.cpp ods-cpp.pdf

corpus.txt: corpus.txt.gz
	gzip -d -k $@.gz

ods-cpp/main.cpp: ods-cpp.tgz
	tar xzf ods-cpp.tgz

ODS_SAMPLE_CODE_URL :=		http://opendatastructures.org/ods-cpp.tgz
ods-cpp.tgz:
	if command -v curl 2>&1 > /dev/null ; then curl -L -O $(ODS_SAMPLE_CODE_URL) ; \
	elif command -v curl 2>&1 > /dev/null ; then wget $(ODS_SAMPLE_CODE_URL) ; fi

ODS_PDF_URL :=			http://opendatastructures.org/ods-cpp.pdf
ods-cpp.pdf:
	if command -v curl 2>&1 > /dev/null ; then curl -L -O $(ODS_PDF_URL) ; \
	elif command -v curl 2>&1 > /dev/null ; then wget $(ODS_PDF_URL) ; fi

