if ! echo ":$PATH:" | grep -q :"ddlog": ; then
    wget https://github.com/vmware/differential-datalog/releases/download/v0.17.4/ddlog-v0.17.4-20200508014413-linux.tar.gz -O ddlog.tar.gz
    tar -xzf ddlog.tar.gz
    
    echo "Adding DDLog to path."
    echo "export DDLOG_HOME=~/.ddlog" >> .profile
    echo "export PATH=$DDLOG_HOME/bin:$PATH" >> .profile
fi