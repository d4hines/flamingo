if ! echo ":$PATH:" | grep -q :"ddlog": ; then
    echo "Fetching DDLog..."
    wget -q https://github.com/vmware/differential-datalog/releases/download/v0.17.4/ddlog-v0.17.4-20200508014413-linux.tar.gz -O ddlog.tar.gz
    echo "Unpacking DDLog binary..."
    tar -xzf ddlog.tar.gz
    mv ddlog ~/.ddlog
    rm ddlog.tar.gz
    
    echo "Adding DDLog to path."
    echo "export DDLOG_HOME=~/.ddlog" >> ~/.profile
    echo "export PATH=\$DDLOG_HOME/bin:\$PATH" >> ~/.profile
    echo "Done"
fi
