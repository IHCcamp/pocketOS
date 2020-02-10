PocketOS
========

How to Build and Flash
----------------------

- Install AtomVM to /usr/local using `cmake`, `make` and `make install`.
- erlang OTP/20, 21 or 22 is required.
- esp-idf v3.2 is required for ESP32 code.


```
cd esp32
git clone https://github.com/bettio/AtomVM.git

cd components/
git clone https://github.com/IHCcamp/gnuboy.git
cd ..

make flash
cd ..
```

```
cd core
cmake -DATOMVM_INSTALL_PREFIX=/usr/local/ .
make core
$IDF_PATH/components/esptool_py/esptool/esptool.py --chip esp32 --port /dev/ttyUSB0 --baud 115200 --before default_reset --after hard_reset write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect  0x110000 pocketos.avm
cd ..
```

```
cd simulator
cmake -DATOMVM_INSTALL_PREFIX=/usr/local/ .
make
```

How to Run The Simulator
------------------------

```
cd simulator
AtomVM ../core/pocketos.avm
```
