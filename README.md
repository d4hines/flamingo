# Getting Started
- Install WSL

    In an administrative PowerShell console, run:
    ```
    Enable-WindowsOptionalFeature -Online -FeatureName Microsoft-Windows-Subsystem-Linux 
    ```
- Install Rust

    You'll need to install it both on Windows and WSL. Below is taken from https://www.rust-lang.org/learn/get-started
    
    For Windows, [download the binary](https://static.rust-lang.org/rustup/dist/x86_64-pc-windows-msvc/rustup-init.exe).

    For WSL, in a WSL terminal, run:
    ```
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    ```
    Be sure to answer its prompts.
- Install DDLog
    In a WSL terminal, run:
    ``` 
    curl https://raw.githubusercontent.com/d4hines/flamingo/pure-rust/install_ddlog.sh | sh
    ```
- Install Flamingo

    In a WSL terminal, run:
    ```
    cargo install flamingo
    ```

