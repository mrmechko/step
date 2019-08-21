MODULE=SkeletonScore
USES=TRIPS.TripsModule TRIPS.KQML TRIPS.util
MAIN=SkeletonScore.py
SRCS=SkeletonScore.py

SUBDIRS=library

CONFIGDIR=../config
include $(CONFIGDIR)/python/prog.mk

.git:
	git init .
	git remote add origin https://github.com/mrmechko/SkeletonScore.git

all:: .git
	git pull origin mini
	git checkout -B mini
	$(PIP) install --user -r requirements.txt

distclean:: clean
	rm -rf .git .bumpversion.cfg .gitignore README.MD SkeletonScore.py predmap.txt requirements.txt

