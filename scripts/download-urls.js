#!/usr/bin/env node

function platforms(version) {
  return {
    "macOS (Apple Silicon)": `UCM.Desktop-${version}-arm64.dmg`,
    "macOS (Intel)": `UCM.Desktop-${version}-x64.dmg`,
    "Windows (exe)": `UCM.Desktop-${version}.Setup.exe`,
    "Windows (msi)": `UCM.Desktop.msi`,
    "RedHat (arm64)": `ucm-desktop-${version}-1.arm64.rpm`,
    "RedHat (x64)": `ucm-desktop-${version}-1.x86_64.rpm`,
    "Debian (arm64)": `ucm-desktop_${version}_arm64.deb`,
    "Debian (x64)": `ucm-desktop_${version}_amd64.deb`,
  };
}

function printDownloadUrls(version, tag) {
  const baseUrl = "https://github.com/unisonweb/ucm-desktop/releases/download";

  for (const [key, file] of Object.entries(platforms(version))) {
    console.log(`* [${key}](${baseUrl}/${tag}/${file})`);
  }
}

// Main execution
if (require.main === module) {
  const version = process.argv[2];
  let tag = process.argv[3];

  if (typeof version === "undefined") {
    console.log(`Usage: node ${process.argv[1]} <version> <tag>`);
    process.exit(1);
  }

  if (!tag) {
    tag = `v${version}`;
  }

  printDownloadUrls(version, tag);
}
