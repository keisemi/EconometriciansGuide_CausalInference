#package_install_test

# remotesパッケージのインストール
install.packages("remotes")

# 本レポジトリに必要な従属パッケージのインストール
remotes::install_deps()

# 本レポジトリをパッケージとしてインストール
remotes::install_github("keisemi/EconometriciansGuide_CausalInference")
