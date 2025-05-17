use zed_extension_api as zed;

struct Bonzai;

impl Bonzai {
    fn language_server_binary_path(
        &self,
        _language_server_id: &zed_extension_api::LanguageServerId,
        worktree: &zed_extension_api::Worktree,
    ) -> zed_extension_api::Result<String> {
        let path = worktree
            .which("bonzai-lsp")
            .ok_or_else(|| "Bonzai LSP must be installed".to_string())?;

        Ok(path)
    }
}

impl zed::Extension for Bonzai {
    fn new() -> Self {
        Bonzai {}
    }

    fn language_server_command(
        &mut self,
        language_server_id: &zed_extension_api::LanguageServerId,
        worktree: &zed_extension_api::Worktree,
    ) -> zed_extension_api::Result<zed_extension_api::Command> {
        Ok(zed::Command {
            command: self.language_server_binary_path(language_server_id, worktree)?,
            args: vec![],
            env: worktree.shell_env(),
        })
    }
}

zed::register_extension!(Bonzai);
