# export JAVA_HOME='/usr/lib/jvm/java-1.11.0-openjdk-amd64'
# export PATH=$JAVA_HOME/bin:$PATH
rm -rf tasks
sbt Universal/packageZipTarball
mkdir tasks
tar -zxf examples/target/universal/examples-0.2.0.tgz -C tasks --strip-components 1
# ./tasks/bin/tdm-initialize-tree --tdmConfFile configs/tdm.conf
# ./tasks/bin/tdm-train-deep-model --tdmConfFile configs/tdm.conf
./tasks/bin/tdm-cluster-tree --tdmConfFile configs/tdm.conf