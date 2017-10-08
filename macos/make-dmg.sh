#!/bin/bash

rm etcd-browser.dmg
cp ../etcd_browser etcd-browser.app/Contents/MacOS/etcd_browser
hdiutil create -srcdir etcd-browser.app etcd-browser.dmg
