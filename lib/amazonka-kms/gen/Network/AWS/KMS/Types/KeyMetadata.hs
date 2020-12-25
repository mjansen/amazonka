{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.KeyMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.KeyMetadata
  ( KeyMetadata (..),

    -- * Smart constructor
    mkKeyMetadata,

    -- * Lenses
    kmKeyId,
    kmAWSAccountId,
    kmArn,
    kmCloudHsmClusterId,
    kmCreationDate,
    kmCustomKeyStoreId,
    kmCustomerMasterKeySpec,
    kmDeletionDate,
    kmDescription,
    kmEnabled,
    kmEncryptionAlgorithms,
    kmExpirationModel,
    kmKeyManager,
    kmKeyState,
    kmKeyUsage,
    kmOrigin,
    kmSigningAlgorithms,
    kmValidTo,
  )
where

import qualified Network.AWS.KMS.Types.AWSAccountId as Types
import qualified Network.AWS.KMS.Types.Arn as Types
import qualified Network.AWS.KMS.Types.CloudHsmClusterId as Types
import qualified Network.AWS.KMS.Types.CustomKeyStoreId as Types
import qualified Network.AWS.KMS.Types.CustomerMasterKeySpec as Types
import qualified Network.AWS.KMS.Types.Description as Types
import qualified Network.AWS.KMS.Types.EncryptionAlgorithmSpec as Types
import qualified Network.AWS.KMS.Types.ExpirationModelType as Types
import qualified Network.AWS.KMS.Types.KeyId as Types
import qualified Network.AWS.KMS.Types.KeyManagerType as Types
import qualified Network.AWS.KMS.Types.KeyState as Types
import qualified Network.AWS.KMS.Types.KeyUsageType as Types
import qualified Network.AWS.KMS.Types.OriginType as Types
import qualified Network.AWS.KMS.Types.SigningAlgorithmSpec as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains metadata about a customer master key (CMK).
--
-- This data type is used as a response element for the 'CreateKey' and 'DescribeKey' operations.
--
-- /See:/ 'mkKeyMetadata' smart constructor.
data KeyMetadata = KeyMetadata'
  { -- | The globally unique identifier for the CMK.
    keyId :: Types.KeyId,
    -- | The twelve-digit account ID of the AWS account that owns the CMK.
    aWSAccountId :: Core.Maybe Types.AWSAccountId,
    -- | The Amazon Resource Name (ARN) of the CMK. For examples, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms AWS Key Management Service (AWS KMS)> in the Example ARNs section of the /AWS General Reference/ .
    arn :: Core.Maybe Types.Arn,
    -- | The cluster ID of the AWS CloudHSM cluster that contains the key material for the CMK. When you create a CMK in a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> , AWS KMS creates the key material for the CMK in the associated AWS CloudHSM cluster. This value is present only when the CMK is created in a custom key store.
    cloudHsmClusterId :: Core.Maybe Types.CloudHsmClusterId,
    -- | The date and time when the CMK was created.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | A unique identifier for the <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> that contains the CMK. This value is present only when the CMK is created in a custom key store.
    customKeyStoreId :: Core.Maybe Types.CustomKeyStoreId,
    -- | Describes the type of key material in the CMK.
    customerMasterKeySpec :: Core.Maybe Types.CustomerMasterKeySpec,
    -- | The date and time after which AWS KMS deletes the CMK. This value is present only when @KeyState@ is @PendingDeletion@ .
    deletionDate :: Core.Maybe Core.NominalDiffTime,
    -- | The description of the CMK.
    description :: Core.Maybe Types.Description,
    -- | Specifies whether the CMK is enabled. When @KeyState@ is @Enabled@ this value is true, otherwise it is false.
    enabled :: Core.Maybe Core.Bool,
    -- | The encryption algorithms that the CMK supports. You cannot use the CMK with other encryption algorithms within AWS KMS.
    --
    -- This field appears only when the @KeyUsage@ of the CMK is @ENCRYPT_DECRYPT@ .
    encryptionAlgorithms :: Core.Maybe [Types.EncryptionAlgorithmSpec],
    -- | Specifies whether the CMK's key material expires. This value is present only when @Origin@ is @EXTERNAL@ , otherwise this value is omitted.
    expirationModel :: Core.Maybe Types.ExpirationModelType,
    -- | The manager of the CMK. CMKs in your AWS account are either customer managed or AWS managed. For more information about the difference, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys> in the /AWS Key Management Service Developer Guide/ .
    keyManager :: Core.Maybe Types.KeyManagerType,
    -- | The current status of the CMK.
    --
    -- For more information about how key state affects the use of a CMK, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key state: Effect on your CMK> in the /AWS Key Management Service Developer Guide/ .
    keyState :: Core.Maybe Types.KeyState,
    -- | The <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> for which you can use the CMK.
    keyUsage :: Core.Maybe Types.KeyUsageType,
    -- | The source of the CMK's key material. When this value is @AWS_KMS@ , AWS KMS created the key material. When this value is @EXTERNAL@ , the key material was imported from your existing key management infrastructure or the CMK lacks key material. When this value is @AWS_CLOUDHSM@ , the key material was created in the AWS CloudHSM cluster associated with a custom key store.
    origin :: Core.Maybe Types.OriginType,
    -- | The signing algorithms that the CMK supports. You cannot use the CMK with other signing algorithms within AWS KMS.
    --
    -- This field appears only when the @KeyUsage@ of the CMK is @SIGN_VERIFY@ .
    signingAlgorithms :: Core.Maybe [Types.SigningAlgorithmSpec],
    -- | The time at which the imported key material expires. When the key material expires, AWS KMS deletes the key material and the CMK becomes unusable. This value is present only for CMKs whose @Origin@ is @EXTERNAL@ and whose @ExpirationModel@ is @KEY_MATERIAL_EXPIRES@ , otherwise this value is omitted.
    validTo :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'KeyMetadata' value with any optional fields omitted.
mkKeyMetadata ::
  -- | 'keyId'
  Types.KeyId ->
  KeyMetadata
mkKeyMetadata keyId =
  KeyMetadata'
    { keyId,
      aWSAccountId = Core.Nothing,
      arn = Core.Nothing,
      cloudHsmClusterId = Core.Nothing,
      creationDate = Core.Nothing,
      customKeyStoreId = Core.Nothing,
      customerMasterKeySpec = Core.Nothing,
      deletionDate = Core.Nothing,
      description = Core.Nothing,
      enabled = Core.Nothing,
      encryptionAlgorithms = Core.Nothing,
      expirationModel = Core.Nothing,
      keyManager = Core.Nothing,
      keyState = Core.Nothing,
      keyUsage = Core.Nothing,
      origin = Core.Nothing,
      signingAlgorithms = Core.Nothing,
      validTo = Core.Nothing
    }

-- | The globally unique identifier for the CMK.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmKeyId :: Lens.Lens' KeyMetadata Types.KeyId
kmKeyId = Lens.field @"keyId"
{-# DEPRECATED kmKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The twelve-digit account ID of the AWS account that owns the CMK.
--
-- /Note:/ Consider using 'aWSAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmAWSAccountId :: Lens.Lens' KeyMetadata (Core.Maybe Types.AWSAccountId)
kmAWSAccountId = Lens.field @"aWSAccountId"
{-# DEPRECATED kmAWSAccountId "Use generic-lens or generic-optics with 'aWSAccountId' instead." #-}

-- | The Amazon Resource Name (ARN) of the CMK. For examples, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms AWS Key Management Service (AWS KMS)> in the Example ARNs section of the /AWS General Reference/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmArn :: Lens.Lens' KeyMetadata (Core.Maybe Types.Arn)
kmArn = Lens.field @"arn"
{-# DEPRECATED kmArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The cluster ID of the AWS CloudHSM cluster that contains the key material for the CMK. When you create a CMK in a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> , AWS KMS creates the key material for the CMK in the associated AWS CloudHSM cluster. This value is present only when the CMK is created in a custom key store.
--
-- /Note:/ Consider using 'cloudHsmClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmCloudHsmClusterId :: Lens.Lens' KeyMetadata (Core.Maybe Types.CloudHsmClusterId)
kmCloudHsmClusterId = Lens.field @"cloudHsmClusterId"
{-# DEPRECATED kmCloudHsmClusterId "Use generic-lens or generic-optics with 'cloudHsmClusterId' instead." #-}

-- | The date and time when the CMK was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmCreationDate :: Lens.Lens' KeyMetadata (Core.Maybe Core.NominalDiffTime)
kmCreationDate = Lens.field @"creationDate"
{-# DEPRECATED kmCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | A unique identifier for the <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> that contains the CMK. This value is present only when the CMK is created in a custom key store.
--
-- /Note:/ Consider using 'customKeyStoreId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmCustomKeyStoreId :: Lens.Lens' KeyMetadata (Core.Maybe Types.CustomKeyStoreId)
kmCustomKeyStoreId = Lens.field @"customKeyStoreId"
{-# DEPRECATED kmCustomKeyStoreId "Use generic-lens or generic-optics with 'customKeyStoreId' instead." #-}

-- | Describes the type of key material in the CMK.
--
-- /Note:/ Consider using 'customerMasterKeySpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmCustomerMasterKeySpec :: Lens.Lens' KeyMetadata (Core.Maybe Types.CustomerMasterKeySpec)
kmCustomerMasterKeySpec = Lens.field @"customerMasterKeySpec"
{-# DEPRECATED kmCustomerMasterKeySpec "Use generic-lens or generic-optics with 'customerMasterKeySpec' instead." #-}

-- | The date and time after which AWS KMS deletes the CMK. This value is present only when @KeyState@ is @PendingDeletion@ .
--
-- /Note:/ Consider using 'deletionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmDeletionDate :: Lens.Lens' KeyMetadata (Core.Maybe Core.NominalDiffTime)
kmDeletionDate = Lens.field @"deletionDate"
{-# DEPRECATED kmDeletionDate "Use generic-lens or generic-optics with 'deletionDate' instead." #-}

-- | The description of the CMK.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmDescription :: Lens.Lens' KeyMetadata (Core.Maybe Types.Description)
kmDescription = Lens.field @"description"
{-# DEPRECATED kmDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies whether the CMK is enabled. When @KeyState@ is @Enabled@ this value is true, otherwise it is false.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmEnabled :: Lens.Lens' KeyMetadata (Core.Maybe Core.Bool)
kmEnabled = Lens.field @"enabled"
{-# DEPRECATED kmEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The encryption algorithms that the CMK supports. You cannot use the CMK with other encryption algorithms within AWS KMS.
--
-- This field appears only when the @KeyUsage@ of the CMK is @ENCRYPT_DECRYPT@ .
--
-- /Note:/ Consider using 'encryptionAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmEncryptionAlgorithms :: Lens.Lens' KeyMetadata (Core.Maybe [Types.EncryptionAlgorithmSpec])
kmEncryptionAlgorithms = Lens.field @"encryptionAlgorithms"
{-# DEPRECATED kmEncryptionAlgorithms "Use generic-lens or generic-optics with 'encryptionAlgorithms' instead." #-}

-- | Specifies whether the CMK's key material expires. This value is present only when @Origin@ is @EXTERNAL@ , otherwise this value is omitted.
--
-- /Note:/ Consider using 'expirationModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmExpirationModel :: Lens.Lens' KeyMetadata (Core.Maybe Types.ExpirationModelType)
kmExpirationModel = Lens.field @"expirationModel"
{-# DEPRECATED kmExpirationModel "Use generic-lens or generic-optics with 'expirationModel' instead." #-}

-- | The manager of the CMK. CMKs in your AWS account are either customer managed or AWS managed. For more information about the difference, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'keyManager' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmKeyManager :: Lens.Lens' KeyMetadata (Core.Maybe Types.KeyManagerType)
kmKeyManager = Lens.field @"keyManager"
{-# DEPRECATED kmKeyManager "Use generic-lens or generic-optics with 'keyManager' instead." #-}

-- | The current status of the CMK.
--
-- For more information about how key state affects the use of a CMK, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key state: Effect on your CMK> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'keyState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmKeyState :: Lens.Lens' KeyMetadata (Core.Maybe Types.KeyState)
kmKeyState = Lens.field @"keyState"
{-# DEPRECATED kmKeyState "Use generic-lens or generic-optics with 'keyState' instead." #-}

-- | The <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> for which you can use the CMK.
--
-- /Note:/ Consider using 'keyUsage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmKeyUsage :: Lens.Lens' KeyMetadata (Core.Maybe Types.KeyUsageType)
kmKeyUsage = Lens.field @"keyUsage"
{-# DEPRECATED kmKeyUsage "Use generic-lens or generic-optics with 'keyUsage' instead." #-}

-- | The source of the CMK's key material. When this value is @AWS_KMS@ , AWS KMS created the key material. When this value is @EXTERNAL@ , the key material was imported from your existing key management infrastructure or the CMK lacks key material. When this value is @AWS_CLOUDHSM@ , the key material was created in the AWS CloudHSM cluster associated with a custom key store.
--
-- /Note:/ Consider using 'origin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmOrigin :: Lens.Lens' KeyMetadata (Core.Maybe Types.OriginType)
kmOrigin = Lens.field @"origin"
{-# DEPRECATED kmOrigin "Use generic-lens or generic-optics with 'origin' instead." #-}

-- | The signing algorithms that the CMK supports. You cannot use the CMK with other signing algorithms within AWS KMS.
--
-- This field appears only when the @KeyUsage@ of the CMK is @SIGN_VERIFY@ .
--
-- /Note:/ Consider using 'signingAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmSigningAlgorithms :: Lens.Lens' KeyMetadata (Core.Maybe [Types.SigningAlgorithmSpec])
kmSigningAlgorithms = Lens.field @"signingAlgorithms"
{-# DEPRECATED kmSigningAlgorithms "Use generic-lens or generic-optics with 'signingAlgorithms' instead." #-}

-- | The time at which the imported key material expires. When the key material expires, AWS KMS deletes the key material and the CMK becomes unusable. This value is present only for CMKs whose @Origin@ is @EXTERNAL@ and whose @ExpirationModel@ is @KEY_MATERIAL_EXPIRES@ , otherwise this value is omitted.
--
-- /Note:/ Consider using 'validTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmValidTo :: Lens.Lens' KeyMetadata (Core.Maybe Core.NominalDiffTime)
kmValidTo = Lens.field @"validTo"
{-# DEPRECATED kmValidTo "Use generic-lens or generic-optics with 'validTo' instead." #-}

instance Core.FromJSON KeyMetadata where
  parseJSON =
    Core.withObject "KeyMetadata" Core.$
      \x ->
        KeyMetadata'
          Core.<$> (x Core..: "KeyId")
          Core.<*> (x Core..:? "AWSAccountId")
          Core.<*> (x Core..:? "Arn")
          Core.<*> (x Core..:? "CloudHsmClusterId")
          Core.<*> (x Core..:? "CreationDate")
          Core.<*> (x Core..:? "CustomKeyStoreId")
          Core.<*> (x Core..:? "CustomerMasterKeySpec")
          Core.<*> (x Core..:? "DeletionDate")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "Enabled")
          Core.<*> (x Core..:? "EncryptionAlgorithms")
          Core.<*> (x Core..:? "ExpirationModel")
          Core.<*> (x Core..:? "KeyManager")
          Core.<*> (x Core..:? "KeyState")
          Core.<*> (x Core..:? "KeyUsage")
          Core.<*> (x Core..:? "Origin")
          Core.<*> (x Core..:? "SigningAlgorithms")
          Core.<*> (x Core..:? "ValidTo")
