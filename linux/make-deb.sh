#!/bin/bash

mkdir -p etcd-browser_0.1/usr/local/bin
mkdir -p etcd-browser_0.1/usr/share/applications
mkdir -p etcd-browser_0.1/usr/share/icons/hicolor/48x48/apps
mkdir -p etcd-browser_0.1/usr/share/icons/hicolor/128x128/apps
cp ../etcd_browser etcd-browser_0.1/usr/local/bin
cp etcd-browser.desktop etcd-browser_0.1/usr/share/applications/
cp ../icons/icon_48.png etcd-browser_0.1/usr/share/icons/hicolor/48x48/apps/etcd-browser.png
cp ../icons/icon_128.png etcd-browser_0.1/usr/share/icons/hicolor/128x128/apps/etcd-browser.png
chown -R root:root etcd-browser_0.1/usr
dpkg-deb --build etcd-browser_0.1