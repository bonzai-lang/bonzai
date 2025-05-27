---
title: Installing Bonzai
description: Follow the instructions to install Bonzai on your computer.
---

Installing Bonzai results in two options: 
- Installing the latest release version (could contain missing features)
- Installing the latest development version (could contain bugs)

> **Note:** Bonzai is not currently available on Windows. We plan to support Windows in the future, but for now, you can use Bonzai on macOS and Linux.

## Installing the latest release version

To install the latest release version of Bonzai:

1. You may fetch it from the [releases page](https://github.com/bonzai-lang/bonzai/releases).
2. Download the appropriate binary for your operating system.
3. Extract the binary from the downloaded archive.
4. Move the folder into a directory that will be added to your `PATH` environment variable.
5. Edit your shell configuration file (e.g., `.bashrc`, `.zshrc`, etc.) to include the directory where you placed the Bonzai binary:

    ```bash
    export BONZAI_PATH="/path/to/bonzai"
    export PATH="$BONZAI_PATH/bin:$PATH"
    ```
6. Reload your shell configuration file:

    ```bash
    source ~/.bashrc  # or source ~/.zshrc
    ```

7. Verify the installation by running:

    ```bash
    bonzai # should throw an error
    ```

## Installing the latest development version

To install the latest development version of Bonzai, you will need to have [Haskell](https://www.haskell.org/downloads/) and [Cabal](https://www.haskell.org/cabal/) installed on your system. Once you have those installed, follow these steps:
1. Clone the Bonzai repository:

    ```bash
    git clone https://github.com/bonzai-lang/bonzai.git
    ```

2. Navigate to the cloned directory:

    ```bash
    cd bonzai
    ```

3. Build the project using Cabal:

    ```bash
    python scripts/build.py
    ```

4. After the build is complete, you can install Bonzai by editing your shell configuration file (e.g., `.bashrc`, `.zshrc`, etc.) to include the directory where you placed the Bonzai binary:

    ```bash
    export BONZAI_PATH="/path/to/bonzai"
    export PATH="$BONZAI_PATH/bin:$PATH"
    ```

5. Reload your shell configuration file:

    ```bash
    source ~/.bashrc  # or source ~/.zshrc
    ```

6. Verify the installation by running:

    ```bash
    bonzai # should throw an error
    ```


## Troubleshooting

If you encounter any issues during the installation process, here are some common troubleshooting steps:
- Ensure that you have the correct permissions to execute the Bonzai binary.
- Make sure that the directory where you placed the Bonzai binary is included in your `PATH` environment variable.
- If you are using a shell other than Bash or Zsh, make sure to edit the appropriate shell configuration file.
- If you are using the development version, ensure that you have the required dependencies installed and that you have built the project successfully.

If you continue to experience issues, please post an issue on the [Bonzai GitHub repository](https://github.com/bonzai-lang/bonzai/issues).