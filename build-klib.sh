
cd ../klib
sbt package
cd ../SlyceFp

cp ../klib/target/scala-2.12/klib_2.12-0.0.1.jar slyce-core/lib
