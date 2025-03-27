use serde::{Deserialize, Serialize};

fn enabled() -> bool {
    true
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Asn1Edition {
    #[serde(rename = "X.208")]
    X208,
    #[serde(rename = "X.680")]
    X680,
}

impl Default for Asn1Edition {
    fn default() -> Self {
        Self::X680
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum EmptyExportBehavior {
    ExportNone,
    ExportAll,
}

impl Default for EmptyExportBehavior {
    fn default() -> Self {
        Self::ExportAll
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct CompilerConfig {
    #[serde(default)]
    pub edition: Asn1Edition,
    #[serde(rename = "emptyExportBehavior", default)]
    pub empty_export_behavior: EmptyExportBehavior,
    #[serde(rename = "permitLowercaseStringIndicator", default = "enabled")]
    pub permit_lowercase_string_indicator: bool,
    #[serde(default = "enabled")]
    pub verify: bool,
}

impl CompilerConfig {
    pub fn from_json(json: &str) -> serde_json::Result<CompilerConfig> {
        serde_json::from_str(json)
    }
}

impl Default for CompilerConfig {
    fn default() -> Self {
        Self::from_json("{}").unwrap()
    }
}
