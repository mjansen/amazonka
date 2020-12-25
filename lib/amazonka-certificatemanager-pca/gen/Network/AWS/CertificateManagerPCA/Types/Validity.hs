{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.Validity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.Validity
  ( Validity (..),

    -- * Smart constructor
    mkValidity,

    -- * Lenses
    vValue,
    vType,
  )
where

import qualified Network.AWS.CertificateManagerPCA.Types.ValidityPeriodType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Validity specifies the period of time during which a certificate is valid. Validity can be expressed as an explicit date and time when the certificate expires, or as a span of time after issuance, stated in days, months, or years. For more information, see <https://tools.ietf.org/html/rfc5280#section-4.1.2.5 Validity> in RFC 5280.
--
-- You can issue a certificate by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_IssueCertificate.html IssueCertificate> action.
--
-- /See:/ 'mkValidity' smart constructor.
data Validity = Validity'
  { -- | A long integer interpreted according to the value of @Type@ , below.
    value :: Core.Natural,
    -- | Determines how /ACM Private CA/ interprets the @Value@ parameter, an integer. Supported validity types include those listed below. Type definitions with values include a sample input value and the resulting output.
    --
    -- @END_DATE@ : The specific date and time when the certificate will expire, expressed using UTCTime (YYMMDDHHMMSS) or GeneralizedTime (YYYYMMDDHHMMSS) format. When UTCTime is used, if the year field (YY) is greater than or equal to 50, the year is interpreted as 19YY. If the year field is less than 50, the year is interpreted as 20YY.
    --
    --     * Sample input value: 491231235959 (UTCTime format)
    --
    --
    --     * Output expiration date/time: 12/31/2049 23:59:59
    --
    --
    -- @ABSOLUTE@ : The specific date and time when the certificate will expire, expressed in seconds since the Unix Epoch.
    --
    --     * Sample input value: 2524608000
    --
    --
    --     * Output expiration date/time: 01/01/2050 00:00:00
    --
    --
    -- @DAYS@ , @MONTHS@ , @YEARS@ : The relative time from the moment of issuance until the certificate will expire, expressed in days, months, or years.
    -- Example if @DAYS@ , issued on 10/12/2020 at 12:34:54 UTC:
    --
    --     * Sample input value: 90
    --
    --
    --     * Output expiration date: 01/10/2020 12:34:54 UTC
    type' :: Types.ValidityPeriodType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Validity' value with any optional fields omitted.
mkValidity ::
  -- | 'value'
  Core.Natural ->
  -- | 'type\''
  Types.ValidityPeriodType ->
  Validity
mkValidity value type' = Validity' {value, type'}

-- | A long integer interpreted according to the value of @Type@ , below.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vValue :: Lens.Lens' Validity Core.Natural
vValue = Lens.field @"value"
{-# DEPRECATED vValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | Determines how /ACM Private CA/ interprets the @Value@ parameter, an integer. Supported validity types include those listed below. Type definitions with values include a sample input value and the resulting output.
--
-- @END_DATE@ : The specific date and time when the certificate will expire, expressed using UTCTime (YYMMDDHHMMSS) or GeneralizedTime (YYYYMMDDHHMMSS) format. When UTCTime is used, if the year field (YY) is greater than or equal to 50, the year is interpreted as 19YY. If the year field is less than 50, the year is interpreted as 20YY.
--
--     * Sample input value: 491231235959 (UTCTime format)
--
--
--     * Output expiration date/time: 12/31/2049 23:59:59
--
--
-- @ABSOLUTE@ : The specific date and time when the certificate will expire, expressed in seconds since the Unix Epoch.
--
--     * Sample input value: 2524608000
--
--
--     * Output expiration date/time: 01/01/2050 00:00:00
--
--
-- @DAYS@ , @MONTHS@ , @YEARS@ : The relative time from the moment of issuance until the certificate will expire, expressed in days, months, or years.
-- Example if @DAYS@ , issued on 10/12/2020 at 12:34:54 UTC:
--
--     * Sample input value: 90
--
--
--     * Output expiration date: 01/10/2020 12:34:54 UTC
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vType :: Lens.Lens' Validity Types.ValidityPeriodType
vType = Lens.field @"type'"
{-# DEPRECATED vType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON Validity where
  toJSON Validity {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Value" Core..= value),
            Core.Just ("Type" Core..= type')
          ]
      )
