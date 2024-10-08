
<!-- README.md is generated from README.Rmd. Please edit that file -->

# 川口康平・澤田真行『因果推論の計量経済学』（日本評論社、2024年刊）のサポート情報提供

<!-- badges: start -->
<!-- badges: end -->

## はじめに

ここは、川口康平・澤田真行『[因果推論の計量経済学](https://www.nippyo.co.jp/shop/book/9359.html)』（日本評論社、2024年刊）のリポジトリです（本書の「はしがき」「目次」「序章」は【[コチラ](https://www.nippyo.co.jp/shop/img/content_pdf/09359.pdf)】からご覧いただけます）。
Rコードの提供や、本文の解説に対する数学的議論（証明、補足など）をまとめた「[テクニカルノート](https://github.com/keisemi/EconometriciansGuide_CausalInference/tree/main/Technical_Note)」の提供を行っています。
また、本書の正誤情報もアップロードしています（「[Errata](https://github.com/keisemi/EconometriciansGuide_CausalInference/tree/main/Errata)」フォルダをご参照ください）。

### ご利用に際してのお断り

なお、本GitHubリポジトリや本書（『因果推論の計量経済学』）の内容、およびサンプルコード等の資料は、情報提供のみを目的としています。運用に際しては十分にご確認をいただき、お客様ご自身の責任とご判断に基づいて行ってください。これらの情報を運用した結果により損害等が生じた場合でも、日本評論社・著者はいかなる責任も負うことはできませんので、ご留意ください。

本リポジトリ提供資料等における誤記や内容面・コーディング上の誤りなどにお気づきの場合は、本GitHub上においてissue投稿やpull
requestを通じてご連絡いただければ幸いです。今後の改訂の参考とさせていただきます（すべてには対応できない場合がございますので、ご了承ください）。
ただし、お客様の利用環境やRのパッケージなどに関する問題に関するご質問には、日本評論社・著者ともに原則としてお答えできません。
また、GitHub以外（お電話、メールなど）を通じたお問合せには、日本評論社・著者ともに原則としてお答えできませんので、あらかじめご了承ください。

## 利用方法

本リポジトリにて提供のRコードは以下の環境で動作確認をしています：

- Windows 11
- R version 4.4.1
- RStudio 2024.04.2

分析コードは

- Rフォルダに格納されている、データ生成などを記述した.Rファイル
- mainフォルダに格納されている、分析実装などを記述した.Rmdファイル

に分かれています。
mainフォルダには分析実装の.Rmdコードの他、それらをknitしたhtmlファイルが含まれています。

ご利用にあたっては、以下の手順が必要となります。

1.  本レポジトリのクローン（ダウンロード）
2.  レポジトリに含まれるCausalInferenceTextbook.RprojをRStudioで開く
3.  CausalInferenceTextbookプロジェクト下で、以下のコードを実行

<!-- -->

    # remotesパッケージのインストール
    install.packages("remotes")
    # 本レポジトリに必要な従属パッケージのインストール
    remotes::install_deps()
    # 本レポジトリをパッケージとしてインストール
    remotes::install_github("keisemi/EconometriciansGuide_CausalInference")

以上を行えば、mainフォルダの各.Rmdファイルを実行できるようになっているはずです。

ただし、一部のパッケージのインストールにあたってコンパイルが必要な場合があります。
(特に、inference_wild_cluster_bootstrap.Rmdでインストールするfwildclusterboot)
その際、環境によっては別途コンパイラのインストールが必要になる場合があります。
例えば、windowsであればRtools (<https://cran.r-project.org/bin/windows/Rtools/>)、
Macであれば、Xcodeをインストールすることが必要となる場合があります。
