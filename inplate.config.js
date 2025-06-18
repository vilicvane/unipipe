import * as FS from 'node:fs';

import TOML from 'smol-toml';

const rootCargoTOML = TOML.parse(FS.readFileSync('Cargo.toml', 'utf-8'));

export const ignore = 'target/**';

export default {
  '*/Cargo.toml': {
    data: rootCargoTOML,
  },
};
