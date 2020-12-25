{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchReadSuccessfulResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchReadSuccessfulResponse
  ( BatchReadSuccessfulResponse (..),

    -- * Smart constructor
    mkBatchReadSuccessfulResponse,

    -- * Lenses
    brsrGetLinkAttributes,
    brsrGetObjectAttributes,
    brsrGetObjectInformation,
    brsrListAttachedIndices,
    brsrListIncomingTypedLinks,
    brsrListIndex,
    brsrListObjectAttributes,
    brsrListObjectChildren,
    brsrListObjectParentPaths,
    brsrListObjectParents,
    brsrListObjectPolicies,
    brsrListOutgoingTypedLinks,
    brsrListPolicyAttachments,
    brsrLookupPolicy,
  )
where

import qualified Network.AWS.CloudDirectory.Types.BatchGetLinkAttributesResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchGetObjectAttributesResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchGetObjectInformationResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchListAttachedIndicesResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchListIncomingTypedLinksResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchListIndexResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchListObjectAttributesResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchListObjectChildrenResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchListObjectParentPathsResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchListObjectParentsResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchListObjectPoliciesResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinksResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchListPolicyAttachmentsResponse as Types
import qualified Network.AWS.CloudDirectory.Types.BatchLookupPolicyResponse as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a @BatchRead@ success response operation.
--
-- /See:/ 'mkBatchReadSuccessfulResponse' smart constructor.
data BatchReadSuccessfulResponse = BatchReadSuccessfulResponse'
  { -- | The list of attributes to retrieve from the typed link.
    getLinkAttributes :: Core.Maybe Types.BatchGetLinkAttributesResponse,
    -- | Retrieves attributes within a facet that are associated with an object.
    getObjectAttributes :: Core.Maybe Types.BatchGetObjectAttributesResponse,
    -- | Retrieves metadata about an object.
    getObjectInformation :: Core.Maybe Types.BatchGetObjectInformationResponse,
    -- | Lists indices attached to an object.
    listAttachedIndices :: Core.Maybe Types.BatchListAttachedIndicesResponse,
    -- | Returns a paginated list of all the incoming 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
    listIncomingTypedLinks :: Core.Maybe Types.BatchListIncomingTypedLinksResponse,
    -- | Lists objects attached to the specified index.
    listIndex :: Core.Maybe Types.BatchListIndexResponse,
    -- | Lists all attributes that are associated with an object.
    listObjectAttributes :: Core.Maybe Types.BatchListObjectAttributesResponse,
    -- | Returns a paginated list of child objects that are associated with a given object.
    listObjectChildren :: Core.Maybe Types.BatchListObjectChildrenResponse,
    -- | Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directorystructure.html Directory Structure> .
    listObjectParentPaths :: Core.Maybe Types.BatchListObjectParentPathsResponse,
    listObjectParents :: Core.Maybe Types.BatchListObjectParentsResponse,
    -- | Returns policies attached to an object in pagination fashion.
    listObjectPolicies :: Core.Maybe Types.BatchListObjectPoliciesResponse,
    -- | Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
    listOutgoingTypedLinks :: Core.Maybe Types.BatchListOutgoingTypedLinksResponse,
    -- | Returns all of the @ObjectIdentifiers@ to which a given policy is attached.
    listPolicyAttachments :: Core.Maybe Types.BatchListPolicyAttachmentsResponse,
    -- | Lists all policies from the root of the 'Directory' to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the @ObjectIdentifier@ for such objects. If policies are present, it returns @ObjectIdentifier@ , @policyId@ , and @policyType@ . Paths that don't lead to the root from the target object are ignored. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
    lookupPolicy :: Core.Maybe Types.BatchLookupPolicyResponse
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchReadSuccessfulResponse' value with any optional fields omitted.
mkBatchReadSuccessfulResponse ::
  BatchReadSuccessfulResponse
mkBatchReadSuccessfulResponse =
  BatchReadSuccessfulResponse'
    { getLinkAttributes = Core.Nothing,
      getObjectAttributes = Core.Nothing,
      getObjectInformation = Core.Nothing,
      listAttachedIndices = Core.Nothing,
      listIncomingTypedLinks = Core.Nothing,
      listIndex = Core.Nothing,
      listObjectAttributes = Core.Nothing,
      listObjectChildren = Core.Nothing,
      listObjectParentPaths = Core.Nothing,
      listObjectParents = Core.Nothing,
      listObjectPolicies = Core.Nothing,
      listOutgoingTypedLinks = Core.Nothing,
      listPolicyAttachments = Core.Nothing,
      lookupPolicy = Core.Nothing
    }

-- | The list of attributes to retrieve from the typed link.
--
-- /Note:/ Consider using 'getLinkAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsrGetLinkAttributes :: Lens.Lens' BatchReadSuccessfulResponse (Core.Maybe Types.BatchGetLinkAttributesResponse)
brsrGetLinkAttributes = Lens.field @"getLinkAttributes"
{-# DEPRECATED brsrGetLinkAttributes "Use generic-lens or generic-optics with 'getLinkAttributes' instead." #-}

-- | Retrieves attributes within a facet that are associated with an object.
--
-- /Note:/ Consider using 'getObjectAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsrGetObjectAttributes :: Lens.Lens' BatchReadSuccessfulResponse (Core.Maybe Types.BatchGetObjectAttributesResponse)
brsrGetObjectAttributes = Lens.field @"getObjectAttributes"
{-# DEPRECATED brsrGetObjectAttributes "Use generic-lens or generic-optics with 'getObjectAttributes' instead." #-}

-- | Retrieves metadata about an object.
--
-- /Note:/ Consider using 'getObjectInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsrGetObjectInformation :: Lens.Lens' BatchReadSuccessfulResponse (Core.Maybe Types.BatchGetObjectInformationResponse)
brsrGetObjectInformation = Lens.field @"getObjectInformation"
{-# DEPRECATED brsrGetObjectInformation "Use generic-lens or generic-optics with 'getObjectInformation' instead." #-}

-- | Lists indices attached to an object.
--
-- /Note:/ Consider using 'listAttachedIndices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsrListAttachedIndices :: Lens.Lens' BatchReadSuccessfulResponse (Core.Maybe Types.BatchListAttachedIndicesResponse)
brsrListAttachedIndices = Lens.field @"listAttachedIndices"
{-# DEPRECATED brsrListAttachedIndices "Use generic-lens or generic-optics with 'listAttachedIndices' instead." #-}

-- | Returns a paginated list of all the incoming 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'listIncomingTypedLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsrListIncomingTypedLinks :: Lens.Lens' BatchReadSuccessfulResponse (Core.Maybe Types.BatchListIncomingTypedLinksResponse)
brsrListIncomingTypedLinks = Lens.field @"listIncomingTypedLinks"
{-# DEPRECATED brsrListIncomingTypedLinks "Use generic-lens or generic-optics with 'listIncomingTypedLinks' instead." #-}

-- | Lists objects attached to the specified index.
--
-- /Note:/ Consider using 'listIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsrListIndex :: Lens.Lens' BatchReadSuccessfulResponse (Core.Maybe Types.BatchListIndexResponse)
brsrListIndex = Lens.field @"listIndex"
{-# DEPRECATED brsrListIndex "Use generic-lens or generic-optics with 'listIndex' instead." #-}

-- | Lists all attributes that are associated with an object.
--
-- /Note:/ Consider using 'listObjectAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsrListObjectAttributes :: Lens.Lens' BatchReadSuccessfulResponse (Core.Maybe Types.BatchListObjectAttributesResponse)
brsrListObjectAttributes = Lens.field @"listObjectAttributes"
{-# DEPRECATED brsrListObjectAttributes "Use generic-lens or generic-optics with 'listObjectAttributes' instead." #-}

-- | Returns a paginated list of child objects that are associated with a given object.
--
-- /Note:/ Consider using 'listObjectChildren' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsrListObjectChildren :: Lens.Lens' BatchReadSuccessfulResponse (Core.Maybe Types.BatchListObjectChildrenResponse)
brsrListObjectChildren = Lens.field @"listObjectChildren"
{-# DEPRECATED brsrListObjectChildren "Use generic-lens or generic-optics with 'listObjectChildren' instead." #-}

-- | Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directorystructure.html Directory Structure> .
--
-- /Note:/ Consider using 'listObjectParentPaths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsrListObjectParentPaths :: Lens.Lens' BatchReadSuccessfulResponse (Core.Maybe Types.BatchListObjectParentPathsResponse)
brsrListObjectParentPaths = Lens.field @"listObjectParentPaths"
{-# DEPRECATED brsrListObjectParentPaths "Use generic-lens or generic-optics with 'listObjectParentPaths' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'listObjectParents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsrListObjectParents :: Lens.Lens' BatchReadSuccessfulResponse (Core.Maybe Types.BatchListObjectParentsResponse)
brsrListObjectParents = Lens.field @"listObjectParents"
{-# DEPRECATED brsrListObjectParents "Use generic-lens or generic-optics with 'listObjectParents' instead." #-}

-- | Returns policies attached to an object in pagination fashion.
--
-- /Note:/ Consider using 'listObjectPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsrListObjectPolicies :: Lens.Lens' BatchReadSuccessfulResponse (Core.Maybe Types.BatchListObjectPoliciesResponse)
brsrListObjectPolicies = Lens.field @"listObjectPolicies"
{-# DEPRECATED brsrListObjectPolicies "Use generic-lens or generic-optics with 'listObjectPolicies' instead." #-}

-- | Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'listOutgoingTypedLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsrListOutgoingTypedLinks :: Lens.Lens' BatchReadSuccessfulResponse (Core.Maybe Types.BatchListOutgoingTypedLinksResponse)
brsrListOutgoingTypedLinks = Lens.field @"listOutgoingTypedLinks"
{-# DEPRECATED brsrListOutgoingTypedLinks "Use generic-lens or generic-optics with 'listOutgoingTypedLinks' instead." #-}

-- | Returns all of the @ObjectIdentifiers@ to which a given policy is attached.
--
-- /Note:/ Consider using 'listPolicyAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsrListPolicyAttachments :: Lens.Lens' BatchReadSuccessfulResponse (Core.Maybe Types.BatchListPolicyAttachmentsResponse)
brsrListPolicyAttachments = Lens.field @"listPolicyAttachments"
{-# DEPRECATED brsrListPolicyAttachments "Use generic-lens or generic-optics with 'listPolicyAttachments' instead." #-}

-- | Lists all policies from the root of the 'Directory' to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the @ObjectIdentifier@ for such objects. If policies are present, it returns @ObjectIdentifier@ , @policyId@ , and @policyType@ . Paths that don't lead to the root from the target object are ignored. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
--
-- /Note:/ Consider using 'lookupPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsrLookupPolicy :: Lens.Lens' BatchReadSuccessfulResponse (Core.Maybe Types.BatchLookupPolicyResponse)
brsrLookupPolicy = Lens.field @"lookupPolicy"
{-# DEPRECATED brsrLookupPolicy "Use generic-lens or generic-optics with 'lookupPolicy' instead." #-}

instance Core.FromJSON BatchReadSuccessfulResponse where
  parseJSON =
    Core.withObject "BatchReadSuccessfulResponse" Core.$
      \x ->
        BatchReadSuccessfulResponse'
          Core.<$> (x Core..:? "GetLinkAttributes")
          Core.<*> (x Core..:? "GetObjectAttributes")
          Core.<*> (x Core..:? "GetObjectInformation")
          Core.<*> (x Core..:? "ListAttachedIndices")
          Core.<*> (x Core..:? "ListIncomingTypedLinks")
          Core.<*> (x Core..:? "ListIndex")
          Core.<*> (x Core..:? "ListObjectAttributes")
          Core.<*> (x Core..:? "ListObjectChildren")
          Core.<*> (x Core..:? "ListObjectParentPaths")
          Core.<*> (x Core..:? "ListObjectParents")
          Core.<*> (x Core..:? "ListObjectPolicies")
          Core.<*> (x Core..:? "ListOutgoingTypedLinks")
          Core.<*> (x Core..:? "ListPolicyAttachments")
          Core.<*> (x Core..:? "LookupPolicy")
