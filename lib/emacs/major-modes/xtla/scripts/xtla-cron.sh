#! /bin/bash
# arch-tag: 66ab59c1-6281-4c11-9b5f-bf95fff9c8bb

# Generates the tarball and html documentation, upload it to gna.org
# This file is currently used only by Matthieu MOY, and is provided
# here only as an example. Copy it and modify it if you wish to use it.

cd `dirname $0`/..
exec > ++xtla-cron.log
make tarball
scp -i ~/.ssh/gna_org xtla-snapshot.tar.gz moy@download.gna.org:/upload/xtla-el
make -C texinfo xtla.html
scp -i ~/.ssh/gna_org texinfo/xtla.html moy@download.gna.org:/upload/xtla-el/docs/xtla-snapshot.html

