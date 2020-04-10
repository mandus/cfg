#!/bin/bash

CURDIR=$(pwd)

if [[ -x $HOME/software/ly ]] ; then
	cd $HOME/software/ly
	git pull
else
	mkdir -p $HOME/software/
	cd $HOME/software

	# clone and build the ly display manager
	git clone https://github.com/cylgom/ly.git

	cd ly
fi

make github
make
sudo make install

# try to automatically disable lightdm
ps ax | grep -v grep | grep -q '/sbin/lightdm' && sudo systemctl disable lightdm.service
ps ax | grep -v grep | grep -q '/sbin/gdm3' && sudo systemctl disable gdm3.service
ps ax | grep -v grep | grep -q '/systemd ' && sudo systemctl enable ly.service && sudo systemctl disable getty@tty2.service

cd ${CURDIR}
