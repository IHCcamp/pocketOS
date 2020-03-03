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
mkdir avm_deps
cp /usr/local/lib/AtomVM/ebin/*.beam avm_deps
mix atomvm.esp32.flash
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
