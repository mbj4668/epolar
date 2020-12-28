-record(prow,
        {bsp :: epolar:speed(),
         vmg :: epolar:speed() | 'undefined',
         aws :: epolar:speed() | 'undefined',
         awa :: epolar:speed() | 'undefined',
         heel :: epolar:angle() | 'undefined',
         reef :: epolar:ratio() | 'undefined',
         flat :: epolar:ratio() | 'undefined'}).
