{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB instance.
module Network.AWS.RDS.CreateDBInstance
  ( -- * Creating a request
    CreateDBInstance (..),
    mkCreateDBInstance,

    -- ** Request lenses
    cdbiDBInstanceIdentifier,
    cdbiDBInstanceClass,
    cdbiEngine,
    cdbiAllocatedStorage,
    cdbiAutoMinorVersionUpgrade,
    cdbiAvailabilityZone,
    cdbiBackupRetentionPeriod,
    cdbiCharacterSetName,
    cdbiCopyTagsToSnapshot,
    cdbiDBClusterIdentifier,
    cdbiDBName,
    cdbiDBParameterGroupName,
    cdbiDBSecurityGroups,
    cdbiDBSubnetGroupName,
    cdbiDeletionProtection,
    cdbiDomain,
    cdbiDomainIAMRoleName,
    cdbiEnableCloudwatchLogsExports,
    cdbiEnableIAMDatabaseAuthentication,
    cdbiEnablePerformanceInsights,
    cdbiEngineVersion,
    cdbiIops,
    cdbiKmsKeyId,
    cdbiLicenseModel,
    cdbiMasterUserPassword,
    cdbiMasterUsername,
    cdbiMaxAllocatedStorage,
    cdbiMonitoringInterval,
    cdbiMonitoringRoleArn,
    cdbiMultiAZ,
    cdbiNcharCharacterSetName,
    cdbiOptionGroupName,
    cdbiPerformanceInsightsKMSKeyId,
    cdbiPerformanceInsightsRetentionPeriod,
    cdbiPort,
    cdbiPreferredBackupWindow,
    cdbiPreferredMaintenanceWindow,
    cdbiProcessorFeatures,
    cdbiPromotionTier,
    cdbiPubliclyAccessible,
    cdbiStorageEncrypted,
    cdbiStorageType,
    cdbiTags,
    cdbiTdeCredentialArn,
    cdbiTdeCredentialPassword,
    cdbiTimezone,
    cdbiVpcSecurityGroupIds,

    -- * Destructuring the response
    CreateDBInstanceResponse (..),
    mkCreateDBInstanceResponse,

    -- ** Response lenses
    cdbirrsDBInstance,
    cdbirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkCreateDBInstance' smart constructor.
data CreateDBInstance = CreateDBInstance'
  { -- | The DB instance identifier. This parameter is stored as a lowercase string.
    --
    -- Constraints:
    --
    --     * Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens.
    --
    --
    -- Example: @mydbinstance@
    dBInstanceIdentifier :: Types.String,
    -- | The compute and memory capacity of the DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./
    dBInstanceClass :: Types.String,
    -- | The name of the database engine to be used for this instance.
    --
    -- Not every database engine is available for every AWS Region.
    -- Valid Values:
    --
    --     * @aurora@ (for MySQL 5.6-compatible Aurora)
    --
    --
    --     * @aurora-mysql@ (for MySQL 5.7-compatible Aurora)
    --
    --
    --     * @aurora-postgresql@
    --
    --
    --     * @mariadb@
    --
    --
    --     * @mysql@
    --
    --
    --     * @oracle-ee@
    --
    --
    --     * @oracle-se2@
    --
    --
    --     * @oracle-se1@
    --
    --
    --     * @oracle-se@
    --
    --
    --     * @postgres@
    --
    --
    --     * @sqlserver-ee@
    --
    --
    --     * @sqlserver-se@
    --
    --
    --     * @sqlserver-ex@
    --
    --
    --     * @sqlserver-web@
    engine :: Types.String,
    -- | The amount of storage (in gibibytes) to allocate for the DB instance.
    --
    -- Type: Integer
    -- __Amazon Aurora__
    -- Not applicable. Aurora cluster volumes automatically grow as the amount of data in your database increases, though you are only charged for the space that you use in an Aurora cluster volume.
    -- __MySQL__
    -- Constraints to the amount of storage for each storage type are the following:
    --
    --     * General Purpose (SSD) storage (gp2): Must be an integer from 20 to 65536.
    --
    --
    --     * Provisioned IOPS storage (io1): Must be an integer from 100 to 65536.
    --
    --
    --     * Magnetic storage (standard): Must be an integer from 5 to 3072.
    --
    --
    -- __MariaDB__
    -- Constraints to the amount of storage for each storage type are the following:
    --
    --     * General Purpose (SSD) storage (gp2): Must be an integer from 20 to 65536.
    --
    --
    --     * Provisioned IOPS storage (io1): Must be an integer from 100 to 65536.
    --
    --
    --     * Magnetic storage (standard): Must be an integer from 5 to 3072.
    --
    --
    -- __PostgreSQL__
    -- Constraints to the amount of storage for each storage type are the following:
    --
    --     * General Purpose (SSD) storage (gp2): Must be an integer from 20 to 65536.
    --
    --
    --     * Provisioned IOPS storage (io1): Must be an integer from 100 to 65536.
    --
    --
    --     * Magnetic storage (standard): Must be an integer from 5 to 3072.
    --
    --
    -- __Oracle__
    -- Constraints to the amount of storage for each storage type are the following:
    --
    --     * General Purpose (SSD) storage (gp2): Must be an integer from 20 to 65536.
    --
    --
    --     * Provisioned IOPS storage (io1): Must be an integer from 100 to 65536.
    --
    --
    --     * Magnetic storage (standard): Must be an integer from 10 to 3072.
    --
    --
    -- __SQL Server__
    -- Constraints to the amount of storage for each storage type are the following:
    --
    --     * General Purpose (SSD) storage (gp2):
    --
    --     * Enterprise and Standard editions: Must be an integer from 200 to 16384.
    --
    --
    --     * Web and Express editions: Must be an integer from 20 to 16384.
    --
    --
    --
    --
    --     * Provisioned IOPS storage (io1):
    --
    --     * Enterprise and Standard editions: Must be an integer from 200 to 16384.
    --
    --
    --     * Web and Express editions: Must be an integer from 100 to 16384.
    --
    --
    --
    --
    --     * Magnetic storage (standard):
    --
    --     * Enterprise and Standard editions: Must be an integer from 200 to 1024.
    --
    --
    --     * Web and Express editions: Must be an integer from 20 to 1024.
    allocatedStorage :: Core.Maybe Core.Int,
    -- | A value that indicates whether minor engine upgrades are applied automatically to the DB instance during the maintenance window. By default, minor engine upgrades are applied automatically.
    autoMinorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | The Availability Zone (AZ) where the database will be created. For information on AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones> .
    --
    -- Default: A random, system-chosen Availability Zone in the endpoint's AWS Region.
    -- Example: @us-east-1d@
    -- Constraint: The @AvailabilityZone@ parameter can't be specified if the DB instance is a Multi-AZ deployment. The specified Availability Zone must be in the same AWS Region as the current endpoint.
    availabilityZone :: Core.Maybe Types.String,
    -- | The number of days for which automated backups are retained. Setting this parameter to a positive number enables backups. Setting this parameter to 0 disables automated backups.
    --
    -- __Amazon Aurora__
    -- Not applicable. The retention period for automated backups is managed by the DB cluster.
    -- Default: 1
    -- Constraints:
    --
    --     * Must be a value from 0 to 35
    --
    --
    --     * Can't be set to 0 if the DB instance is a source to read replicas
    backupRetentionPeriod :: Core.Maybe Core.Int,
    -- | For supported engines, indicates that the DB instance should be associated with the specified CharacterSet.
    --
    -- __Amazon Aurora__
    -- Not applicable. The character set is managed by the DB cluster. For more information, see @CreateDBCluster@ .
    characterSetName :: Core.Maybe Types.String,
    -- | A value that indicates whether to copy tags from the DB instance to snapshots of the DB instance. By default, tags are not copied.
    --
    -- __Amazon Aurora__
    -- Not applicable. Copying tags to snapshots is managed by the DB cluster. Setting this value for an Aurora DB instance has no effect on the DB cluster setting.
    copyTagsToSnapshot :: Core.Maybe Core.Bool,
    -- | The identifier of the DB cluster that the instance will belong to.
    dBClusterIdentifier :: Core.Maybe Types.String,
    -- | The meaning of this parameter differs according to the database engine you use.
    --
    -- __MySQL__
    -- The name of the database to create when the DB instance is created. If this parameter isn't specified, no database is created in the DB instance.
    -- Constraints:
    --
    --     * Must contain 1 to 64 letters or numbers.
    --
    --
    --     * Must begin with a letter. Subsequent characters can be letters, underscores, or digits (0-9).
    --
    --
    --     * Can't be a word reserved by the specified database engine
    --
    --
    -- __MariaDB__
    -- The name of the database to create when the DB instance is created. If this parameter isn't specified, no database is created in the DB instance.
    -- Constraints:
    --
    --     * Must contain 1 to 64 letters or numbers.
    --
    --
    --     * Must begin with a letter. Subsequent characters can be letters, underscores, or digits (0-9).
    --
    --
    --     * Can't be a word reserved by the specified database engine
    --
    --
    -- __PostgreSQL__
    -- The name of the database to create when the DB instance is created. If this parameter isn't specified, the default "postgres" database is created in the DB instance.
    -- Constraints:
    --
    --     * Must contain 1 to 63 letters, numbers, or underscores.
    --
    --
    --     * Must begin with a letter. Subsequent characters can be letters, underscores, or digits (0-9).
    --
    --
    --     * Can't be a word reserved by the specified database engine
    --
    --
    -- __Oracle__
    -- The Oracle System ID (SID) of the created DB instance. If you specify @null@ , the default value @ORCL@ is used. You can't specify the string NULL, or any other reserved word, for @DBName@ .
    -- Default: @ORCL@
    -- Constraints:
    --
    --     * Can't be longer than 8 characters
    --
    --
    -- __SQL Server__
    -- Not applicable. Must be null.
    -- __Amazon Aurora__
    -- The name of the database to create when the primary instance of the DB cluster is created. If this parameter isn't specified, no database is created in the DB instance.
    -- Constraints:
    --
    --     * Must contain 1 to 64 letters or numbers.
    --
    --
    --     * Can't be a word reserved by the specified database engine
    dBName :: Core.Maybe Types.String,
    -- | The name of the DB parameter group to associate with this DB instance. If you do not specify a value, then the default DB parameter group for the specified DB engine and version is used.
    --
    -- Constraints:
    --
    --     * Must be 1 to 255 letters, numbers, or hyphens.
    --
    --
    --     * First character must be a letter
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens
    dBParameterGroupName :: Core.Maybe Types.String,
    -- | A list of DB security groups to associate with this DB instance.
    --
    -- Default: The default DB security group for the database engine.
    dBSecurityGroups :: Core.Maybe [Types.String],
    -- | A DB subnet group to associate with this DB instance.
    --
    -- If there is no DB subnet group, then it is a non-VPC DB instance.
    dBSubnetGroupName :: Core.Maybe Types.String,
    -- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
    --
    -- __Amazon Aurora__
    -- Not applicable. You can enable or disable deletion protection for the DB cluster. For more information, see @CreateDBCluster@ . DB instances in a DB cluster can be deleted even when deletion protection is enabled for the DB cluster.
    deletionProtection :: Core.Maybe Core.Bool,
    -- | The Active Directory directory ID to create the DB instance in. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
    domain :: Core.Maybe Types.String,
    -- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
    domainIAMRoleName :: Core.Maybe Types.String,
    -- | The list of log types that need to be enabled for exporting to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon Relational Database Service User Guide/ .
    --
    -- __Amazon Aurora__
    -- Not applicable. CloudWatch Logs exports are managed by the DB cluster.
    -- __MariaDB__
    -- Possible values are @audit@ , @error@ , @general@ , and @slowquery@ .
    -- __Microsoft SQL Server__
    -- Possible values are @agent@ and @error@ .
    -- __MySQL__
    -- Possible values are @audit@ , @error@ , @general@ , and @slowquery@ .
    -- __Oracle__
    -- Possible values are @alert@ , @audit@ , @listener@ , and @trace@ .
    -- __PostgreSQL__
    -- Possible values are @postgresql@ and @upgrade@ .
    enableCloudwatchLogsExports :: Core.Maybe [Types.String],
    -- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
    --
    -- This setting doesn't apply to Amazon Aurora. Mapping AWS IAM accounts to database accounts is managed by the DB cluster.
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
    enableIAMDatabaseAuthentication :: Core.Maybe Core.Bool,
    -- | A value that indicates whether to enable Performance Insights for the DB instance.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon Relational Database Service User Guide/ .
    enablePerformanceInsights :: Core.Maybe Core.Bool,
    -- | The version number of the database engine to use.
    --
    -- For a list of valid engine versions, use the @DescribeDBEngineVersions@ action.
    -- The following are the database engines and links to information about the major and minor versions that are available with Amazon RDS. Not every database engine is available for every AWS Region.
    -- __Amazon Aurora__
    -- Not applicable. The version number of the database engine to be used by the DB instance is managed by the DB cluster.
    -- __MariaDB__
    -- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MariaDB.html#MariaDB.Concepts.VersionMgmt MariaDB on Amazon RDS Versions> in the /Amazon RDS User Guide./
    -- __Microsoft SQL Server__
    -- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server Versions on Amazon RDS> in the /Amazon RDS User Guide./
    -- __MySQL__
    -- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MySQL.html#MySQL.Concepts.VersionMgmt MySQL on Amazon RDS Versions> in the /Amazon RDS User Guide./
    -- __Oracle__
    -- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Appendix.Oracle.PatchComposition.html Oracle Database Engine Release Notes> in the /Amazon RDS User Guide./
    -- __PostgreSQL__
    -- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_PostgreSQL.html#PostgreSQL.Concepts.General.DBVersions Supported PostgreSQL Database Versions> in the /Amazon RDS User Guide./
    engineVersion :: Core.Maybe Types.String,
    -- | The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance. For information about valid Iops values, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance> in the /Amazon RDS User Guide/ .
    --
    -- Constraints: For MariaDB, MySQL, Oracle, and PostgreSQL DB instances, must be a multiple between .5 and 50 of the storage amount for the DB instance. For SQL Server DB instances, must be a multiple between 1 and 50 of the storage amount for the DB instance.
    iops :: Core.Maybe Core.Int,
    -- | The AWS KMS key identifier for an encrypted DB instance.
    --
    -- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB instance with the same AWS account that owns the KMS encryption key used to encrypt the new DB instance, then you can use the KMS key alias instead of the ARN for the KM encryption key.
    -- __Amazon Aurora__
    -- Not applicable. The KMS key identifier is managed by the DB cluster. For more information, see @CreateDBCluster@ .
    -- If @StorageEncrypted@ is enabled, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
    kmsKeyId :: Core.Maybe Types.String,
    -- | License model information for this DB instance.
    --
    -- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
    licenseModel :: Core.Maybe Types.String,
    -- | The password for the master user. The password can include any printable ASCII character except "/", """, or "@".
    --
    -- __Amazon Aurora__
    -- Not applicable. The password for the master user is managed by the DB cluster.
    -- __MariaDB__
    -- Constraints: Must contain from 8 to 41 characters.
    -- __Microsoft SQL Server__
    -- Constraints: Must contain from 8 to 128 characters.
    -- __MySQL__
    -- Constraints: Must contain from 8 to 41 characters.
    -- __Oracle__
    -- Constraints: Must contain from 8 to 30 characters.
    -- __PostgreSQL__
    -- Constraints: Must contain from 8 to 128 characters.
    masterUserPassword :: Core.Maybe Types.String,
    -- | The name for the master user.
    --
    -- __Amazon Aurora__
    -- Not applicable. The name for the master user is managed by the DB cluster.
    -- __MariaDB__
    -- Constraints:
    --
    --     * Required for MariaDB.
    --
    --
    --     * Must be 1 to 16 letters or numbers.
    --
    --
    --     * Can't be a reserved word for the chosen database engine.
    --
    --
    -- __Microsoft SQL Server__
    -- Constraints:
    --
    --     * Required for SQL Server.
    --
    --
    --     * Must be 1 to 128 letters or numbers.
    --
    --
    --     * The first character must be a letter.
    --
    --
    --     * Can't be a reserved word for the chosen database engine.
    --
    --
    -- __MySQL__
    -- Constraints:
    --
    --     * Required for MySQL.
    --
    --
    --     * Must be 1 to 16 letters or numbers.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Can't be a reserved word for the chosen database engine.
    --
    --
    -- __Oracle__
    -- Constraints:
    --
    --     * Required for Oracle.
    --
    --
    --     * Must be 1 to 30 letters or numbers.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Can't be a reserved word for the chosen database engine.
    --
    --
    -- __PostgreSQL__
    -- Constraints:
    --
    --     * Required for PostgreSQL.
    --
    --
    --     * Must be 1 to 63 letters or numbers.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Can't be a reserved word for the chosen database engine.
    masterUsername :: Core.Maybe Types.String,
    -- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
    maxAllocatedStorage :: Core.Maybe Core.Int,
    -- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0.
    --
    -- If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0.
    -- Valid Values: @0, 1, 5, 10, 15, 30, 60@
    monitoringInterval :: Core.Maybe Core.Int,
    -- | The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring> in the /Amazon RDS User Guide/ .
    --
    -- If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
    monitoringRoleArn :: Core.Maybe Types.String,
    -- | A value that indicates whether the DB instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
    multiAZ :: Core.Maybe Core.Bool,
    -- | The name of the NCHAR character set for the Oracle DB instance.
    ncharCharacterSetName :: Core.Maybe Types.String,
    -- | Indicates that the DB instance should be associated with the specified option group.
    --
    -- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group. Also, that option group can't be removed from a DB instance once it is associated with a DB instance
    optionGroupName :: Core.Maybe Types.String,
    -- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
    --
    -- If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
    performanceInsightsKMSKeyId :: Core.Maybe Types.String,
    -- | The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
    performanceInsightsRetentionPeriod :: Core.Maybe Core.Int,
    -- | The port number on which the database accepts connections.
    --
    -- __MySQL__
    -- Default: @3306@
    -- Valid values: @1150-65535@
    -- Type: Integer
    -- __MariaDB__
    -- Default: @3306@
    -- Valid values: @1150-65535@
    -- Type: Integer
    -- __PostgreSQL__
    -- Default: @5432@
    -- Valid values: @1150-65535@
    -- Type: Integer
    -- __Oracle__
    -- Default: @1521@
    -- Valid values: @1150-65535@
    -- __SQL Server__
    -- Default: @1433@
    -- Valid values: @1150-65535@ except @1234@ , @1434@ , @3260@ , @3343@ , @3389@ , @47001@ , and @49152-49156@ .
    -- __Amazon Aurora__
    -- Default: @3306@
    -- Valid values: @1150-65535@
    -- Type: Integer
    port :: Core.Maybe Core.Int,
    -- | The daily time range during which automated backups are created if automated backups are enabled, using the @BackupRetentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow The Backup Window> in the /Amazon RDS User Guide/ .
    --
    -- __Amazon Aurora__
    -- Not applicable. The daily time range for creating automated backups is managed by the DB cluster.
    -- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow Adjusting the Preferred DB Instance Maintenance Window> in the /Amazon RDS User Guide/ .
    -- Constraints:
    --
    --     * Must be in the format @hh24:mi-hh24:mi@ .
    --
    --
    --     * Must be in Universal Coordinated Time (UTC).
    --
    --
    --     * Must not conflict with the preferred maintenance window.
    --
    --
    --     * Must be at least 30 minutes.
    preferredBackupWindow :: Core.Maybe Types.String,
    -- | The time range each week during which system maintenance can occur, in Universal Coordinated Time (UTC). For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window> .
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    -- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week.
    -- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Core.Maybe Types.String,
    -- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
    processorFeatures :: Core.Maybe [Types.ProcessorFeature],
    -- | A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
    --
    -- Default: 1
    -- Valid Values: 0 - 15
    promotionTier :: Core.Maybe Core.Int,
    -- | A value that indicates whether the DB instance is publicly accessible.
    --
    -- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
    -- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
    -- Default: The default behavior varies depending on whether @DBSubnetGroupName@ is specified.
    -- If @DBSubnetGroupName@ isn't specified, and @PubliclyAccessible@ isn't specified, the following applies:
    --
    --     * If the default VPC in the target region doesn’t have an Internet gateway attached to it, the DB instance is private.
    --
    --
    --     * If the default VPC in the target region has an Internet gateway attached to it, the DB instance is public.
    --
    --
    -- If @DBSubnetGroupName@ is specified, and @PubliclyAccessible@ isn't specified, the following applies:
    --
    --     * If the subnets are part of a VPC that doesn’t have an Internet gateway attached to it, the DB instance is private.
    --
    --
    --     * If the subnets are part of a VPC that has an Internet gateway attached to it, the DB instance is public.
    publiclyAccessible :: Core.Maybe Core.Bool,
    -- | A value that indicates whether the DB instance is encrypted. By default, it isn't encrypted.
    --
    -- __Amazon Aurora__
    -- Not applicable. The encryption for DB instances is managed by the DB cluster.
    storageEncrypted :: Core.Maybe Core.Bool,
    -- | Specifies the storage type to be associated with the DB instance.
    --
    -- Valid values: @standard | gp2 | io1@
    -- If you specify @io1@ , you must also include a value for the @Iops@ parameter.
    -- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
    storageType :: Core.Maybe Types.String,
    -- | Tags to assign to the DB instance.
    tags :: Core.Maybe [Types.Tag],
    -- | The ARN from the key store with which to associate the instance for TDE encryption.
    tdeCredentialArn :: Core.Maybe Types.String,
    -- | The password for the given ARN from the key store in order to access the device.
    tdeCredentialPassword :: Core.Maybe Types.String,
    -- | The time zone of the DB instance. The time zone parameter is currently supported only by <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.TimeZone Microsoft SQL Server> .
    timezone :: Core.Maybe Types.String,
    -- | A list of Amazon EC2 VPC security groups to associate with this DB instance.
    --
    -- __Amazon Aurora__
    -- Not applicable. The associated list of EC2 VPC security groups is managed by the DB cluster.
    -- Default: The default EC2 VPC security group for the DB subnet group's VPC.
    vpcSecurityGroupIds :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBInstance' value with any optional fields omitted.
mkCreateDBInstance ::
  -- | 'dBInstanceIdentifier'
  Types.String ->
  -- | 'dBInstanceClass'
  Types.String ->
  -- | 'engine'
  Types.String ->
  CreateDBInstance
mkCreateDBInstance dBInstanceIdentifier dBInstanceClass engine =
  CreateDBInstance'
    { dBInstanceIdentifier,
      dBInstanceClass,
      engine,
      allocatedStorage = Core.Nothing,
      autoMinorVersionUpgrade = Core.Nothing,
      availabilityZone = Core.Nothing,
      backupRetentionPeriod = Core.Nothing,
      characterSetName = Core.Nothing,
      copyTagsToSnapshot = Core.Nothing,
      dBClusterIdentifier = Core.Nothing,
      dBName = Core.Nothing,
      dBParameterGroupName = Core.Nothing,
      dBSecurityGroups = Core.Nothing,
      dBSubnetGroupName = Core.Nothing,
      deletionProtection = Core.Nothing,
      domain = Core.Nothing,
      domainIAMRoleName = Core.Nothing,
      enableCloudwatchLogsExports = Core.Nothing,
      enableIAMDatabaseAuthentication = Core.Nothing,
      enablePerformanceInsights = Core.Nothing,
      engineVersion = Core.Nothing,
      iops = Core.Nothing,
      kmsKeyId = Core.Nothing,
      licenseModel = Core.Nothing,
      masterUserPassword = Core.Nothing,
      masterUsername = Core.Nothing,
      maxAllocatedStorage = Core.Nothing,
      monitoringInterval = Core.Nothing,
      monitoringRoleArn = Core.Nothing,
      multiAZ = Core.Nothing,
      ncharCharacterSetName = Core.Nothing,
      optionGroupName = Core.Nothing,
      performanceInsightsKMSKeyId = Core.Nothing,
      performanceInsightsRetentionPeriod = Core.Nothing,
      port = Core.Nothing,
      preferredBackupWindow = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      processorFeatures = Core.Nothing,
      promotionTier = Core.Nothing,
      publiclyAccessible = Core.Nothing,
      storageEncrypted = Core.Nothing,
      storageType = Core.Nothing,
      tags = Core.Nothing,
      tdeCredentialArn = Core.Nothing,
      tdeCredentialPassword = Core.Nothing,
      timezone = Core.Nothing,
      vpcSecurityGroupIds = Core.Nothing
    }

-- | The DB instance identifier. This parameter is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain from 1 to 63 letters, numbers, or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
-- Example: @mydbinstance@
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiDBInstanceIdentifier :: Lens.Lens' CreateDBInstance Types.String
cdbiDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# DEPRECATED cdbiDBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead." #-}

-- | The compute and memory capacity of the DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'dBInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiDBInstanceClass :: Lens.Lens' CreateDBInstance Types.String
cdbiDBInstanceClass = Lens.field @"dBInstanceClass"
{-# DEPRECATED cdbiDBInstanceClass "Use generic-lens or generic-optics with 'dBInstanceClass' instead." #-}

-- | The name of the database engine to be used for this instance.
--
-- Not every database engine is available for every AWS Region.
-- Valid Values:
--
--     * @aurora@ (for MySQL 5.6-compatible Aurora)
--
--
--     * @aurora-mysql@ (for MySQL 5.7-compatible Aurora)
--
--
--     * @aurora-postgresql@
--
--
--     * @mariadb@
--
--
--     * @mysql@
--
--
--     * @oracle-ee@
--
--
--     * @oracle-se2@
--
--
--     * @oracle-se1@
--
--
--     * @oracle-se@
--
--
--     * @postgres@
--
--
--     * @sqlserver-ee@
--
--
--     * @sqlserver-se@
--
--
--     * @sqlserver-ex@
--
--
--     * @sqlserver-web@
--
--
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiEngine :: Lens.Lens' CreateDBInstance Types.String
cdbiEngine = Lens.field @"engine"
{-# DEPRECATED cdbiEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The amount of storage (in gibibytes) to allocate for the DB instance.
--
-- Type: Integer
-- __Amazon Aurora__
-- Not applicable. Aurora cluster volumes automatically grow as the amount of data in your database increases, though you are only charged for the space that you use in an Aurora cluster volume.
-- __MySQL__
-- Constraints to the amount of storage for each storage type are the following:
--
--     * General Purpose (SSD) storage (gp2): Must be an integer from 20 to 65536.
--
--
--     * Provisioned IOPS storage (io1): Must be an integer from 100 to 65536.
--
--
--     * Magnetic storage (standard): Must be an integer from 5 to 3072.
--
--
-- __MariaDB__
-- Constraints to the amount of storage for each storage type are the following:
--
--     * General Purpose (SSD) storage (gp2): Must be an integer from 20 to 65536.
--
--
--     * Provisioned IOPS storage (io1): Must be an integer from 100 to 65536.
--
--
--     * Magnetic storage (standard): Must be an integer from 5 to 3072.
--
--
-- __PostgreSQL__
-- Constraints to the amount of storage for each storage type are the following:
--
--     * General Purpose (SSD) storage (gp2): Must be an integer from 20 to 65536.
--
--
--     * Provisioned IOPS storage (io1): Must be an integer from 100 to 65536.
--
--
--     * Magnetic storage (standard): Must be an integer from 5 to 3072.
--
--
-- __Oracle__
-- Constraints to the amount of storage for each storage type are the following:
--
--     * General Purpose (SSD) storage (gp2): Must be an integer from 20 to 65536.
--
--
--     * Provisioned IOPS storage (io1): Must be an integer from 100 to 65536.
--
--
--     * Magnetic storage (standard): Must be an integer from 10 to 3072.
--
--
-- __SQL Server__
-- Constraints to the amount of storage for each storage type are the following:
--
--     * General Purpose (SSD) storage (gp2):
--
--     * Enterprise and Standard editions: Must be an integer from 200 to 16384.
--
--
--     * Web and Express editions: Must be an integer from 20 to 16384.
--
--
--
--
--     * Provisioned IOPS storage (io1):
--
--     * Enterprise and Standard editions: Must be an integer from 200 to 16384.
--
--
--     * Web and Express editions: Must be an integer from 100 to 16384.
--
--
--
--
--     * Magnetic storage (standard):
--
--     * Enterprise and Standard editions: Must be an integer from 200 to 1024.
--
--
--     * Web and Express editions: Must be an integer from 20 to 1024.
--
--
--
--
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiAllocatedStorage :: Lens.Lens' CreateDBInstance (Core.Maybe Core.Int)
cdbiAllocatedStorage = Lens.field @"allocatedStorage"
{-# DEPRECATED cdbiAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | A value that indicates whether minor engine upgrades are applied automatically to the DB instance during the maintenance window. By default, minor engine upgrades are applied automatically.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiAutoMinorVersionUpgrade :: Lens.Lens' CreateDBInstance (Core.Maybe Core.Bool)
cdbiAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# DEPRECATED cdbiAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | The Availability Zone (AZ) where the database will be created. For information on AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones> .
--
-- Default: A random, system-chosen Availability Zone in the endpoint's AWS Region.
-- Example: @us-east-1d@
-- Constraint: The @AvailabilityZone@ parameter can't be specified if the DB instance is a Multi-AZ deployment. The specified Availability Zone must be in the same AWS Region as the current endpoint.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiAvailabilityZone :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED cdbiAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The number of days for which automated backups are retained. Setting this parameter to a positive number enables backups. Setting this parameter to 0 disables automated backups.
--
-- __Amazon Aurora__
-- Not applicable. The retention period for automated backups is managed by the DB cluster.
-- Default: 1
-- Constraints:
--
--     * Must be a value from 0 to 35
--
--
--     * Can't be set to 0 if the DB instance is a source to read replicas
--
--
--
-- /Note:/ Consider using 'backupRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiBackupRetentionPeriod :: Lens.Lens' CreateDBInstance (Core.Maybe Core.Int)
cdbiBackupRetentionPeriod = Lens.field @"backupRetentionPeriod"
{-# DEPRECATED cdbiBackupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead." #-}

-- | For supported engines, indicates that the DB instance should be associated with the specified CharacterSet.
--
-- __Amazon Aurora__
-- Not applicable. The character set is managed by the DB cluster. For more information, see @CreateDBCluster@ .
--
-- /Note:/ Consider using 'characterSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiCharacterSetName :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiCharacterSetName = Lens.field @"characterSetName"
{-# DEPRECATED cdbiCharacterSetName "Use generic-lens or generic-optics with 'characterSetName' instead." #-}

-- | A value that indicates whether to copy tags from the DB instance to snapshots of the DB instance. By default, tags are not copied.
--
-- __Amazon Aurora__
-- Not applicable. Copying tags to snapshots is managed by the DB cluster. Setting this value for an Aurora DB instance has no effect on the DB cluster setting.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiCopyTagsToSnapshot :: Lens.Lens' CreateDBInstance (Core.Maybe Core.Bool)
cdbiCopyTagsToSnapshot = Lens.field @"copyTagsToSnapshot"
{-# DEPRECATED cdbiCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

-- | The identifier of the DB cluster that the instance will belong to.
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiDBClusterIdentifier :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# DEPRECATED cdbiDBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead." #-}

-- | The meaning of this parameter differs according to the database engine you use.
--
-- __MySQL__
-- The name of the database to create when the DB instance is created. If this parameter isn't specified, no database is created in the DB instance.
-- Constraints:
--
--     * Must contain 1 to 64 letters or numbers.
--
--
--     * Must begin with a letter. Subsequent characters can be letters, underscores, or digits (0-9).
--
--
--     * Can't be a word reserved by the specified database engine
--
--
-- __MariaDB__
-- The name of the database to create when the DB instance is created. If this parameter isn't specified, no database is created in the DB instance.
-- Constraints:
--
--     * Must contain 1 to 64 letters or numbers.
--
--
--     * Must begin with a letter. Subsequent characters can be letters, underscores, or digits (0-9).
--
--
--     * Can't be a word reserved by the specified database engine
--
--
-- __PostgreSQL__
-- The name of the database to create when the DB instance is created. If this parameter isn't specified, the default "postgres" database is created in the DB instance.
-- Constraints:
--
--     * Must contain 1 to 63 letters, numbers, or underscores.
--
--
--     * Must begin with a letter. Subsequent characters can be letters, underscores, or digits (0-9).
--
--
--     * Can't be a word reserved by the specified database engine
--
--
-- __Oracle__
-- The Oracle System ID (SID) of the created DB instance. If you specify @null@ , the default value @ORCL@ is used. You can't specify the string NULL, or any other reserved word, for @DBName@ .
-- Default: @ORCL@
-- Constraints:
--
--     * Can't be longer than 8 characters
--
--
-- __SQL Server__
-- Not applicable. Must be null.
-- __Amazon Aurora__
-- The name of the database to create when the primary instance of the DB cluster is created. If this parameter isn't specified, no database is created in the DB instance.
-- Constraints:
--
--     * Must contain 1 to 64 letters or numbers.
--
--
--     * Can't be a word reserved by the specified database engine
--
--
--
-- /Note:/ Consider using 'dBName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiDBName :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiDBName = Lens.field @"dBName"
{-# DEPRECATED cdbiDBName "Use generic-lens or generic-optics with 'dBName' instead." #-}

-- | The name of the DB parameter group to associate with this DB instance. If you do not specify a value, then the default DB parameter group for the specified DB engine and version is used.
--
-- Constraints:
--
--     * Must be 1 to 255 letters, numbers, or hyphens.
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
--
-- /Note:/ Consider using 'dBParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiDBParameterGroupName :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiDBParameterGroupName = Lens.field @"dBParameterGroupName"
{-# DEPRECATED cdbiDBParameterGroupName "Use generic-lens or generic-optics with 'dBParameterGroupName' instead." #-}

-- | A list of DB security groups to associate with this DB instance.
--
-- Default: The default DB security group for the database engine.
--
-- /Note:/ Consider using 'dBSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiDBSecurityGroups :: Lens.Lens' CreateDBInstance (Core.Maybe [Types.String])
cdbiDBSecurityGroups = Lens.field @"dBSecurityGroups"
{-# DEPRECATED cdbiDBSecurityGroups "Use generic-lens or generic-optics with 'dBSecurityGroups' instead." #-}

-- | A DB subnet group to associate with this DB instance.
--
-- If there is no DB subnet group, then it is a non-VPC DB instance.
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiDBSubnetGroupName :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# DEPRECATED cdbiDBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead." #-}

-- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
--
-- __Amazon Aurora__
-- Not applicable. You can enable or disable deletion protection for the DB cluster. For more information, see @CreateDBCluster@ . DB instances in a DB cluster can be deleted even when deletion protection is enabled for the DB cluster.
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiDeletionProtection :: Lens.Lens' CreateDBInstance (Core.Maybe Core.Bool)
cdbiDeletionProtection = Lens.field @"deletionProtection"
{-# DEPRECATED cdbiDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | The Active Directory directory ID to create the DB instance in. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiDomain :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiDomain = Lens.field @"domain"
{-# DEPRECATED cdbiDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiDomainIAMRoleName :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiDomainIAMRoleName = Lens.field @"domainIAMRoleName"
{-# DEPRECATED cdbiDomainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead." #-}

-- | The list of log types that need to be enabled for exporting to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon Relational Database Service User Guide/ .
--
-- __Amazon Aurora__
-- Not applicable. CloudWatch Logs exports are managed by the DB cluster.
-- __MariaDB__
-- Possible values are @audit@ , @error@ , @general@ , and @slowquery@ .
-- __Microsoft SQL Server__
-- Possible values are @agent@ and @error@ .
-- __MySQL__
-- Possible values are @audit@ , @error@ , @general@ , and @slowquery@ .
-- __Oracle__
-- Possible values are @alert@ , @audit@ , @listener@ , and @trace@ .
-- __PostgreSQL__
-- Possible values are @postgresql@ and @upgrade@ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiEnableCloudwatchLogsExports :: Lens.Lens' CreateDBInstance (Core.Maybe [Types.String])
cdbiEnableCloudwatchLogsExports = Lens.field @"enableCloudwatchLogsExports"
{-# DEPRECATED cdbiEnableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead." #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- This setting doesn't apply to Amazon Aurora. Mapping AWS IAM accounts to database accounts is managed by the DB cluster.
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiEnableIAMDatabaseAuthentication :: Lens.Lens' CreateDBInstance (Core.Maybe Core.Bool)
cdbiEnableIAMDatabaseAuthentication = Lens.field @"enableIAMDatabaseAuthentication"
{-# DEPRECATED cdbiEnableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead." #-}

-- | A value that indicates whether to enable Performance Insights for the DB instance.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon Relational Database Service User Guide/ .
--
-- /Note:/ Consider using 'enablePerformanceInsights' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiEnablePerformanceInsights :: Lens.Lens' CreateDBInstance (Core.Maybe Core.Bool)
cdbiEnablePerformanceInsights = Lens.field @"enablePerformanceInsights"
{-# DEPRECATED cdbiEnablePerformanceInsights "Use generic-lens or generic-optics with 'enablePerformanceInsights' instead." #-}

-- | The version number of the database engine to use.
--
-- For a list of valid engine versions, use the @DescribeDBEngineVersions@ action.
-- The following are the database engines and links to information about the major and minor versions that are available with Amazon RDS. Not every database engine is available for every AWS Region.
-- __Amazon Aurora__
-- Not applicable. The version number of the database engine to be used by the DB instance is managed by the DB cluster.
-- __MariaDB__
-- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MariaDB.html#MariaDB.Concepts.VersionMgmt MariaDB on Amazon RDS Versions> in the /Amazon RDS User Guide./
-- __Microsoft SQL Server__
-- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server Versions on Amazon RDS> in the /Amazon RDS User Guide./
-- __MySQL__
-- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MySQL.html#MySQL.Concepts.VersionMgmt MySQL on Amazon RDS Versions> in the /Amazon RDS User Guide./
-- __Oracle__
-- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Appendix.Oracle.PatchComposition.html Oracle Database Engine Release Notes> in the /Amazon RDS User Guide./
-- __PostgreSQL__
-- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_PostgreSQL.html#PostgreSQL.Concepts.General.DBVersions Supported PostgreSQL Database Versions> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiEngineVersion :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED cdbiEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance. For information about valid Iops values, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance> in the /Amazon RDS User Guide/ .
--
-- Constraints: For MariaDB, MySQL, Oracle, and PostgreSQL DB instances, must be a multiple between .5 and 50 of the storage amount for the DB instance. For SQL Server DB instances, must be a multiple between 1 and 50 of the storage amount for the DB instance.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiIops :: Lens.Lens' CreateDBInstance (Core.Maybe Core.Int)
cdbiIops = Lens.field @"iops"
{-# DEPRECATED cdbiIops "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | The AWS KMS key identifier for an encrypted DB instance.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB instance with the same AWS account that owns the KMS encryption key used to encrypt the new DB instance, then you can use the KMS key alias instead of the ARN for the KM encryption key.
-- __Amazon Aurora__
-- Not applicable. The KMS key identifier is managed by the DB cluster. For more information, see @CreateDBCluster@ .
-- If @StorageEncrypted@ is enabled, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiKmsKeyId :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED cdbiKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | License model information for this DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiLicenseModel :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiLicenseModel = Lens.field @"licenseModel"
{-# DEPRECATED cdbiLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | The password for the master user. The password can include any printable ASCII character except "/", """, or "@".
--
-- __Amazon Aurora__
-- Not applicable. The password for the master user is managed by the DB cluster.
-- __MariaDB__
-- Constraints: Must contain from 8 to 41 characters.
-- __Microsoft SQL Server__
-- Constraints: Must contain from 8 to 128 characters.
-- __MySQL__
-- Constraints: Must contain from 8 to 41 characters.
-- __Oracle__
-- Constraints: Must contain from 8 to 30 characters.
-- __PostgreSQL__
-- Constraints: Must contain from 8 to 128 characters.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiMasterUserPassword :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiMasterUserPassword = Lens.field @"masterUserPassword"
{-# DEPRECATED cdbiMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | The name for the master user.
--
-- __Amazon Aurora__
-- Not applicable. The name for the master user is managed by the DB cluster.
-- __MariaDB__
-- Constraints:
--
--     * Required for MariaDB.
--
--
--     * Must be 1 to 16 letters or numbers.
--
--
--     * Can't be a reserved word for the chosen database engine.
--
--
-- __Microsoft SQL Server__
-- Constraints:
--
--     * Required for SQL Server.
--
--
--     * Must be 1 to 128 letters or numbers.
--
--
--     * The first character must be a letter.
--
--
--     * Can't be a reserved word for the chosen database engine.
--
--
-- __MySQL__
-- Constraints:
--
--     * Required for MySQL.
--
--
--     * Must be 1 to 16 letters or numbers.
--
--
--     * First character must be a letter.
--
--
--     * Can't be a reserved word for the chosen database engine.
--
--
-- __Oracle__
-- Constraints:
--
--     * Required for Oracle.
--
--
--     * Must be 1 to 30 letters or numbers.
--
--
--     * First character must be a letter.
--
--
--     * Can't be a reserved word for the chosen database engine.
--
--
-- __PostgreSQL__
-- Constraints:
--
--     * Required for PostgreSQL.
--
--
--     * Must be 1 to 63 letters or numbers.
--
--
--     * First character must be a letter.
--
--
--     * Can't be a reserved word for the chosen database engine.
--
--
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiMasterUsername :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiMasterUsername = Lens.field @"masterUsername"
{-# DEPRECATED cdbiMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
--
-- /Note:/ Consider using 'maxAllocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiMaxAllocatedStorage :: Lens.Lens' CreateDBInstance (Core.Maybe Core.Int)
cdbiMaxAllocatedStorage = Lens.field @"maxAllocatedStorage"
{-# DEPRECATED cdbiMaxAllocatedStorage "Use generic-lens or generic-optics with 'maxAllocatedStorage' instead." #-}

-- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0.
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
--
-- /Note:/ Consider using 'monitoringInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiMonitoringInterval :: Lens.Lens' CreateDBInstance (Core.Maybe Core.Int)
cdbiMonitoringInterval = Lens.field @"monitoringInterval"
{-# DEPRECATED cdbiMonitoringInterval "Use generic-lens or generic-optics with 'monitoringInterval' instead." #-}

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring> in the /Amazon RDS User Guide/ .
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
--
-- /Note:/ Consider using 'monitoringRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiMonitoringRoleArn :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiMonitoringRoleArn = Lens.field @"monitoringRoleArn"
{-# DEPRECATED cdbiMonitoringRoleArn "Use generic-lens or generic-optics with 'monitoringRoleArn' instead." #-}

-- | A value that indicates whether the DB instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiMultiAZ :: Lens.Lens' CreateDBInstance (Core.Maybe Core.Bool)
cdbiMultiAZ = Lens.field @"multiAZ"
{-# DEPRECATED cdbiMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The name of the NCHAR character set for the Oracle DB instance.
--
-- /Note:/ Consider using 'ncharCharacterSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiNcharCharacterSetName :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiNcharCharacterSetName = Lens.field @"ncharCharacterSetName"
{-# DEPRECATED cdbiNcharCharacterSetName "Use generic-lens or generic-optics with 'ncharCharacterSetName' instead." #-}

-- | Indicates that the DB instance should be associated with the specified option group.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group. Also, that option group can't be removed from a DB instance once it is associated with a DB instance
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiOptionGroupName :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiOptionGroupName = Lens.field @"optionGroupName"
{-# DEPRECATED cdbiOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'performanceInsightsKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiPerformanceInsightsKMSKeyId :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiPerformanceInsightsKMSKeyId = Lens.field @"performanceInsightsKMSKeyId"
{-# DEPRECATED cdbiPerformanceInsightsKMSKeyId "Use generic-lens or generic-optics with 'performanceInsightsKMSKeyId' instead." #-}

-- | The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
--
-- /Note:/ Consider using 'performanceInsightsRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiPerformanceInsightsRetentionPeriod :: Lens.Lens' CreateDBInstance (Core.Maybe Core.Int)
cdbiPerformanceInsightsRetentionPeriod = Lens.field @"performanceInsightsRetentionPeriod"
{-# DEPRECATED cdbiPerformanceInsightsRetentionPeriod "Use generic-lens or generic-optics with 'performanceInsightsRetentionPeriod' instead." #-}

-- | The port number on which the database accepts connections.
--
-- __MySQL__
-- Default: @3306@
-- Valid values: @1150-65535@
-- Type: Integer
-- __MariaDB__
-- Default: @3306@
-- Valid values: @1150-65535@
-- Type: Integer
-- __PostgreSQL__
-- Default: @5432@
-- Valid values: @1150-65535@
-- Type: Integer
-- __Oracle__
-- Default: @1521@
-- Valid values: @1150-65535@
-- __SQL Server__
-- Default: @1433@
-- Valid values: @1150-65535@ except @1234@ , @1434@ , @3260@ , @3343@ , @3389@ , @47001@ , and @49152-49156@ .
-- __Amazon Aurora__
-- Default: @3306@
-- Valid values: @1150-65535@
-- Type: Integer
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiPort :: Lens.Lens' CreateDBInstance (Core.Maybe Core.Int)
cdbiPort = Lens.field @"port"
{-# DEPRECATED cdbiPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The daily time range during which automated backups are created if automated backups are enabled, using the @BackupRetentionPeriod@ parameter. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow The Backup Window> in the /Amazon RDS User Guide/ .
--
-- __Amazon Aurora__
-- Not applicable. The daily time range for creating automated backups is managed by the DB cluster.
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow Adjusting the Preferred DB Instance Maintenance Window> in the /Amazon RDS User Guide/ .
-- Constraints:
--
--     * Must be in the format @hh24:mi-hh24:mi@ .
--
--
--     * Must be in Universal Coordinated Time (UTC).
--
--
--     * Must not conflict with the preferred maintenance window.
--
--
--     * Must be at least 30 minutes.
--
--
--
-- /Note:/ Consider using 'preferredBackupWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiPreferredBackupWindow :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiPreferredBackupWindow = Lens.field @"preferredBackupWindow"
{-# DEPRECATED cdbiPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

-- | The time range each week during which system maintenance can occur, in Universal Coordinated Time (UTC). For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window> .
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week.
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
-- Constraints: Minimum 30-minute window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiPreferredMaintenanceWindow :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# DEPRECATED cdbiPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- /Note:/ Consider using 'processorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiProcessorFeatures :: Lens.Lens' CreateDBInstance (Core.Maybe [Types.ProcessorFeature])
cdbiProcessorFeatures = Lens.field @"processorFeatures"
{-# DEPRECATED cdbiProcessorFeatures "Use generic-lens or generic-optics with 'processorFeatures' instead." #-}

-- | A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
--
-- Default: 1
-- Valid Values: 0 - 15
--
-- /Note:/ Consider using 'promotionTier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiPromotionTier :: Lens.Lens' CreateDBInstance (Core.Maybe Core.Int)
cdbiPromotionTier = Lens.field @"promotionTier"
{-# DEPRECATED cdbiPromotionTier "Use generic-lens or generic-optics with 'promotionTier' instead." #-}

-- | A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- Default: The default behavior varies depending on whether @DBSubnetGroupName@ is specified.
-- If @DBSubnetGroupName@ isn't specified, and @PubliclyAccessible@ isn't specified, the following applies:
--
--     * If the default VPC in the target region doesn’t have an Internet gateway attached to it, the DB instance is private.
--
--
--     * If the default VPC in the target region has an Internet gateway attached to it, the DB instance is public.
--
--
-- If @DBSubnetGroupName@ is specified, and @PubliclyAccessible@ isn't specified, the following applies:
--
--     * If the subnets are part of a VPC that doesn’t have an Internet gateway attached to it, the DB instance is private.
--
--
--     * If the subnets are part of a VPC that has an Internet gateway attached to it, the DB instance is public.
--
--
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiPubliclyAccessible :: Lens.Lens' CreateDBInstance (Core.Maybe Core.Bool)
cdbiPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# DEPRECATED cdbiPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | A value that indicates whether the DB instance is encrypted. By default, it isn't encrypted.
--
-- __Amazon Aurora__
-- Not applicable. The encryption for DB instances is managed by the DB cluster.
--
-- /Note:/ Consider using 'storageEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiStorageEncrypted :: Lens.Lens' CreateDBInstance (Core.Maybe Core.Bool)
cdbiStorageEncrypted = Lens.field @"storageEncrypted"
{-# DEPRECATED cdbiStorageEncrypted "Use generic-lens or generic-optics with 'storageEncrypted' instead." #-}

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@
-- If you specify @io1@ , you must also include a value for the @Iops@ parameter.
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiStorageType :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiStorageType = Lens.field @"storageType"
{-# DEPRECATED cdbiStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

-- | Tags to assign to the DB instance.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiTags :: Lens.Lens' CreateDBInstance (Core.Maybe [Types.Tag])
cdbiTags = Lens.field @"tags"
{-# DEPRECATED cdbiTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ARN from the key store with which to associate the instance for TDE encryption.
--
-- /Note:/ Consider using 'tdeCredentialArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiTdeCredentialArn :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiTdeCredentialArn = Lens.field @"tdeCredentialArn"
{-# DEPRECATED cdbiTdeCredentialArn "Use generic-lens or generic-optics with 'tdeCredentialArn' instead." #-}

-- | The password for the given ARN from the key store in order to access the device.
--
-- /Note:/ Consider using 'tdeCredentialPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiTdeCredentialPassword :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiTdeCredentialPassword = Lens.field @"tdeCredentialPassword"
{-# DEPRECATED cdbiTdeCredentialPassword "Use generic-lens or generic-optics with 'tdeCredentialPassword' instead." #-}

-- | The time zone of the DB instance. The time zone parameter is currently supported only by <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.TimeZone Microsoft SQL Server> .
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiTimezone :: Lens.Lens' CreateDBInstance (Core.Maybe Types.String)
cdbiTimezone = Lens.field @"timezone"
{-# DEPRECATED cdbiTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

-- | A list of Amazon EC2 VPC security groups to associate with this DB instance.
--
-- __Amazon Aurora__
-- Not applicable. The associated list of EC2 VPC security groups is managed by the DB cluster.
-- Default: The default EC2 VPC security group for the DB subnet group's VPC.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbiVpcSecurityGroupIds :: Lens.Lens' CreateDBInstance (Core.Maybe [Types.String])
cdbiVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# DEPRECATED cdbiVpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

instance Core.AWSRequest CreateDBInstance where
  type Rs CreateDBInstance = CreateDBInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateDBInstance")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBInstanceIdentifier" dBInstanceIdentifier)
                Core.<> (Core.toQueryValue "DBInstanceClass" dBInstanceClass)
                Core.<> (Core.toQueryValue "Engine" engine)
                Core.<> (Core.toQueryValue "AllocatedStorage" Core.<$> allocatedStorage)
                Core.<> ( Core.toQueryValue "AutoMinorVersionUpgrade"
                            Core.<$> autoMinorVersionUpgrade
                        )
                Core.<> (Core.toQueryValue "AvailabilityZone" Core.<$> availabilityZone)
                Core.<> ( Core.toQueryValue "BackupRetentionPeriod"
                            Core.<$> backupRetentionPeriod
                        )
                Core.<> (Core.toQueryValue "CharacterSetName" Core.<$> characterSetName)
                Core.<> ( Core.toQueryValue "CopyTagsToSnapshot"
                            Core.<$> copyTagsToSnapshot
                        )
                Core.<> ( Core.toQueryValue "DBClusterIdentifier"
                            Core.<$> dBClusterIdentifier
                        )
                Core.<> (Core.toQueryValue "DBName" Core.<$> dBName)
                Core.<> ( Core.toQueryValue "DBParameterGroupName"
                            Core.<$> dBParameterGroupName
                        )
                Core.<> ( Core.toQueryValue
                            "DBSecurityGroups"
                            ( Core.toQueryList "DBSecurityGroupName"
                                Core.<$> dBSecurityGroups
                            )
                        )
                Core.<> (Core.toQueryValue "DBSubnetGroupName" Core.<$> dBSubnetGroupName)
                Core.<> ( Core.toQueryValue "DeletionProtection"
                            Core.<$> deletionProtection
                        )
                Core.<> (Core.toQueryValue "Domain" Core.<$> domain)
                Core.<> (Core.toQueryValue "DomainIAMRoleName" Core.<$> domainIAMRoleName)
                Core.<> ( Core.toQueryValue
                            "EnableCloudwatchLogsExports"
                            (Core.toQueryList "member" Core.<$> enableCloudwatchLogsExports)
                        )
                Core.<> ( Core.toQueryValue "EnableIAMDatabaseAuthentication"
                            Core.<$> enableIAMDatabaseAuthentication
                        )
                Core.<> ( Core.toQueryValue "EnablePerformanceInsights"
                            Core.<$> enablePerformanceInsights
                        )
                Core.<> (Core.toQueryValue "EngineVersion" Core.<$> engineVersion)
                Core.<> (Core.toQueryValue "Iops" Core.<$> iops)
                Core.<> (Core.toQueryValue "KmsKeyId" Core.<$> kmsKeyId)
                Core.<> (Core.toQueryValue "LicenseModel" Core.<$> licenseModel)
                Core.<> ( Core.toQueryValue "MasterUserPassword"
                            Core.<$> masterUserPassword
                        )
                Core.<> (Core.toQueryValue "MasterUsername" Core.<$> masterUsername)
                Core.<> ( Core.toQueryValue "MaxAllocatedStorage"
                            Core.<$> maxAllocatedStorage
                        )
                Core.<> ( Core.toQueryValue "MonitoringInterval"
                            Core.<$> monitoringInterval
                        )
                Core.<> (Core.toQueryValue "MonitoringRoleArn" Core.<$> monitoringRoleArn)
                Core.<> (Core.toQueryValue "MultiAZ" Core.<$> multiAZ)
                Core.<> ( Core.toQueryValue "NcharCharacterSetName"
                            Core.<$> ncharCharacterSetName
                        )
                Core.<> (Core.toQueryValue "OptionGroupName" Core.<$> optionGroupName)
                Core.<> ( Core.toQueryValue "PerformanceInsightsKMSKeyId"
                            Core.<$> performanceInsightsKMSKeyId
                        )
                Core.<> ( Core.toQueryValue "PerformanceInsightsRetentionPeriod"
                            Core.<$> performanceInsightsRetentionPeriod
                        )
                Core.<> (Core.toQueryValue "Port" Core.<$> port)
                Core.<> ( Core.toQueryValue "PreferredBackupWindow"
                            Core.<$> preferredBackupWindow
                        )
                Core.<> ( Core.toQueryValue "PreferredMaintenanceWindow"
                            Core.<$> preferredMaintenanceWindow
                        )
                Core.<> ( Core.toQueryValue
                            "ProcessorFeatures"
                            (Core.toQueryList "ProcessorFeature" Core.<$> processorFeatures)
                        )
                Core.<> (Core.toQueryValue "PromotionTier" Core.<$> promotionTier)
                Core.<> ( Core.toQueryValue "PubliclyAccessible"
                            Core.<$> publiclyAccessible
                        )
                Core.<> (Core.toQueryValue "StorageEncrypted" Core.<$> storageEncrypted)
                Core.<> (Core.toQueryValue "StorageType" Core.<$> storageType)
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" Core.<$> tags))
                Core.<> (Core.toQueryValue "TdeCredentialArn" Core.<$> tdeCredentialArn)
                Core.<> ( Core.toQueryValue "TdeCredentialPassword"
                            Core.<$> tdeCredentialPassword
                        )
                Core.<> (Core.toQueryValue "Timezone" Core.<$> timezone)
                Core.<> ( Core.toQueryValue
                            "VpcSecurityGroupIds"
                            ( Core.toQueryList "VpcSecurityGroupId"
                                Core.<$> vpcSecurityGroupIds
                            )
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateDBInstanceResult"
      ( \s h x ->
          CreateDBInstanceResponse'
            Core.<$> (x Core..@? "DBInstance") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateDBInstanceResponse' smart constructor.
data CreateDBInstanceResponse = CreateDBInstanceResponse'
  { dBInstance :: Core.Maybe Types.DBInstance,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateDBInstanceResponse' value with any optional fields omitted.
mkCreateDBInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDBInstanceResponse
mkCreateDBInstanceResponse responseStatus =
  CreateDBInstanceResponse'
    { dBInstance = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrsDBInstance :: Lens.Lens' CreateDBInstanceResponse (Core.Maybe Types.DBInstance)
cdbirrsDBInstance = Lens.field @"dBInstance"
{-# DEPRECATED cdbirrsDBInstance "Use generic-lens or generic-optics with 'dBInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrsResponseStatus :: Lens.Lens' CreateDBInstanceResponse Core.Int
cdbirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdbirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
