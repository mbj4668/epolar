-record(prow,
        {bsp :: polar:speed(),
         vmg :: polar:speed() | 'undefined',
         aws :: polar:speed() | 'undefined',
         awa :: polar:speed() | 'undefined',
         heel :: polar:angle() | 'undefined',
         reef :: polar:ratio() | 'undefined',
         flat :: polar:ratio() | 'undefined'}).
