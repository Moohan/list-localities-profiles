## 2024-05-20 - Quoting File Paths
**Learning:** The `run_in_bash_session` and `read_file` tools will fail if the file path contains spaces and is not enclosed in double quotes. This is a common and easy-to-forget mistake.
**Action:** Always enclose file paths in double quotes, especially when they are constructed from directory and file names that might contain spaces. For example: `"my path/my file.R"`.
