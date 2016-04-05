# Generator ideal results

* types
    * category
        * basetype
            * XML: `<type category="basetype">typedef <type>uint32_t</type> <name>VkSampleMask</name>;</type>`
            * C: `typedef uint32_t VkSampleMask;`
            * Rust: `type VkSampleMask = uint32_t;`
        * bitmask
            * XML: `<type category="bitmask">typedef <type>VkFlags</type> <name>VkFramebufferCreateFlags</name>;</type>`
            * C: `typedef VkFlags VkFramebufferCreateFlags;`
            * Rust: `type VkFramebufferCreateFlags = VkFlags;`
        * define
            * XML:
   
              ```xml
              <type category="define">#define <name>VK_MAKE_VERSION</name>(major, minor, patch) \
                    (((major) &lt;&lt; 22) | ((minor) &lt;&lt; 12) | (patch))</type>
              ```
            * C: 
            
              ```c
              #define VK_MAKE_VERSION(major, minor, patch) \
                  (((major) << 22) | ((minor) << 12) | (patch))
              ```
            * Rust:
            
              ```rust
                macro_rules! VK_MAKE_VERSION {
                    ($major:expr, $minor: expr, $patch:expr) => {
                        ((($major) << 22) | (($minor) << 12) | ($patch))
                    }
                }
              ```

            * XML: `<type category="define">#define <name>VK_NULL_HANDLE</name> 0</type>`
            * C: `#define VK_NULL_HANDLE 0`
            * Rust:
            
              ```rust
                macro_rules! VK_NULL_HANDLE {
                    () => {
                        0
                    }
                }
              ```

            * If macro cannot be translated or has conditionals, abort macro creation and attempt to build rest of document.
            * If the macro is used elsewhere in the XML file, evaluate it when building rust bindings. Otherwise, translate it
              into actual macro for others to use
        * enum
            * Ignored
        * funcpointer
        * group
            * Not currently present
        * handle
            * Expands with build-time macros
        * include
            * Ignored
        * struct
            * XML:
            
                ```xml
                <type category="struct" name="VkAttachmentDescription">
                    <member optional="true"><type>VkAttachmentDescriptionFlags</type> <name>flags</name></member>
                    <member><type>VkFormat</type>               <name>format</name></member>
                    <member><type>VkSampleCountFlagBits</type>  <name>samples</name></member>
                    <member><type>VkAttachmentLoadOp</type>     <name>loadOp</name></member>                         <!-- Load operation for color or depth data -->
                    <member><type>VkAttachmentStoreOp</type>    <name>storeOp</name></member>                        <!-- Store operation for color or depth data -->
                    <member><type>VkAttachmentLoadOp</type>     <name>stencilLoadOp</name></member>                  <!-- Load operation for stencil data -->
                    <member><type>VkAttachmentStoreOp</type>    <name>stencilStoreOp</name></member>                 <!-- Store operation for stencil data -->
                    <member><type>VkImageLayout</type>          <name>initialLayout</name></member>
                    <member><type>VkImageLayout</type>          <name>finalLayout</name></member>
                </type>
                ```
            * C:
            
                ```C
                typedef struct VkAttachmentDescription {
                    VkAttachmentDescriptionFlags    flags;
                    VkFormat                        format;
                    VkSampleCountFlagBits           samples;
                    VkAttachmentLoadOp              loadOp;
                    VkAttachmentStoreOp             storeOp;
                    VkAttachmentLoadOp              stencilLoadOp;
                    VkAttachmentStoreOp             stencilStoreOp;
                    VkImageLayout                   initialLayout;
                    VkImageLayout                   finalLayout;
                } VkAttachmentDescription;
                ```
            * Rust:
            
                ```rust
                pub struct VkAttachmentDescription {
                    flags: VkAttachmentDescriptionFlags,
                    format: VkFormat,
                    samples: VkSampleCountFlagBits,
                    loadOp: VkAttachmentLoadOp,
                    storeOp: VkAttachmentStoreOp,
                    stencilLoadOp: VkAttachmentLoadOp,
                    stencilStoreOp: VkAttachmentStoreOp,
                    initialLayout: VkImageLayout,
                    finalLayout: VkImageLayout,
                }
                ```
        * union
            * XML:
                ```xml
                <type category="union" name="VkClearColorValue" comment="// Union allowing specification of floating point, integer, or unsigned integer color data. Actual union selected is based on image/attachment being cleared.">
                    <member><type>float</type>                  <name>float32</name>[4]</member>
                    <member><type>int32_t</type>                <name>int32</name>[4]</member>
                    <member><type>uint32_t</type>               <name>uint32</name>[4]</member>
                </type>
                ```
            * C:
            
                ```C
                typedef union VkClearColorValue {
                    float       float32[4];
                    int32_t     int32[4];
                    uint32_t    uint32[4];
                } VkClearColorValue;
                ```
            * Rust:
            
                ```rust
                pub struct VkClearColorValue {
                    /// Represents the raw data. Because unions in C are untagged, we go through all of the union's
                    /// possible variants and check which one has the largest size. Because the maximum size, in this
                    /// case, is 32 bytes we create an array of `u8`s that is 32 bytes long.
                    data: [u8; 32]
                }

                impl VkClearColorValue {
                    pub unsafe fn float32(&self) -> &[c_float; 4] {
                        mem::transmute(&self.data)
                    }

                    pub unsafe fn int32(&self) -> &[int32_t; 4] {
                        mem::transmute(&self.data)
                    }

                    pub unsafe fn uint32(&self) -> &[uint32_t; 4] {
                        mem::transmute(&self.data)
                    }

                    pub unsafe fn float32_mut(&mut self) -> &mut [c_float; 4] {
                        mem::transmute(&mut self.data)
                    }

                    pub unsafe fn int32_mut(&mut self) -> &mut [int32_t; 4] {
                        mem::transmute(&mut self.data)
                    }

                    pub unsafe fn uint32_mut(&mut self) -> &mut [uint32_t; 4] {
                        mem::transmute(&mut self.data)
                    }

                    pub unsafe fn new_float32(float32: c_float) -> VkClearColorValue {
                        let mut union = mem::zeroed();
                        union.data = mem::transmute_copy(&float32);
                        union
                    }

                    pub unsafe fn new_int32(int32: int32_t) -> VkClearColorValue {
                        let mut union = mem::zeroed();
                        union.data = mem::transmute_copy(&float32);
                        union
                    }

                    pub unsafe fn new_uint32(uint32: uint32_t) -> VkClearColorValue {
                        let mut union = mem::zeroed();
                        union.data = mem::transmute_copy(&float32);
                        union
                    }
                }
                ```
