require glfw/glfw_boost
require opengl/opengl_boost
require opengl/opengl_gen
require glsl/geom_gen
require glsl/glsl_common
require glsl/glsl_internal
require daslib/defer
require daslib/safe_addr
require daslib/math_boost
require math
require imgui
require imgui_app
require imgui/imgui_boost

var [[in, location=0]] v_position : float3
var [[in, location=1]] v_normal   : float3
var [[in, location=2]] v_texcoord : float2
var [[uniform]] v_model : float4x4
var [[uniform]] v_view : float4x4
var [[uniform]] v_projection : float4x4
var [[uniform]] f_crop_plane_x : float = -0.5
var [[uniform]] f_crop_plane_y : float = -0.5
var [[uniform]] f_crop_plane_x_max : float = 0.5
var [[uniform]] f_crop_plane_y_max : float = 0.5
var [[uniform]] f_camera_position_rotated : float3
var [[inout]] f_texcoord : float2
var [[inout]] f_normal : float3
var [[inout]] f_position : float3
var [[inout]] f_worldpos : float3

var [[uniform, stage=0]] f_tex : sampler2D
var [[out]] f_FragColor : float4

[vertex_program]
def vs_preview
    f_texcoord = v_texcoord
    f_normal = normalize ( float3x3(v_model) * v_normal )
    let tpos = v_model * float4(v_position, 1.0)
    f_position = tpos.xyz
    f_worldpos = v_position
    gl_Position = v_projection * v_view * tpos

[fragment_program (version=400)]
def fs_preview
    if (f_worldpos.x < f_crop_plane_x) || (f_worldpos.y < f_crop_plane_y) || (f_worldpos.x > f_crop_plane_x_max) || (f_worldpos.y > f_crop_plane_y_max)
        discard()
    let pS = 24.0

    let light_dir = normalize(float3(1, 2,-3))
    let camera_dir = normalize(f_position)
    let normal = normalize(f_normal)
    let k = -dot(light_dir,normal)
    let diffuse = float3(saturate(k))
    let refl = normalize ( reflect(camera_dir,normal) )
    let kS = saturate(dot(-light_dir,refl))
    let specular = float3(pow(kS,pS)*kS)
    f_FragColor.xyz = texture(f_tex,f_texcoord).xyz * diffuse + specular
    f_FragColor.w = 1.

[fragment_program (version=400)]
def fs_preview_front
    //let not_inside_camera =  (f_worldpos.x < f_crop_plane_x) || (f_worldpos.y < f_crop_plane_y) || (f_worldpos.x > f_crop_plane_x_max) || (f_worldpos.y > f_crop_plane_y_max)
    let not_inside_camera = (f_worldpos.x < f_crop_plane_x) && (f_camera_position_rotated.x > -f_crop_plane_x) || (f_worldpos.y < f_crop_plane_y) && (f_camera_position_rotated.y < f_crop_plane_y) || (f_worldpos.x > f_crop_plane_x_max) && (f_camera_position_rotated.x < -f_crop_plane_x_max) || (f_worldpos.y > f_crop_plane_y_max) && (f_camera_position_rotated.y > f_crop_plane_y_max)
    if not_inside_camera
        discard()
    f_FragColor = float4(0.0, 0.0, 0.0, 1.0)

[fragment_program (version=400)]
def fs_preview_caps
    f_FragColor = float4(0.0, 1.0, 0.0, 1.0)

def gen_axis_rect(plt : GenDirection; x1, x2, y1, y2, coord: float)
    var frag : GeometryFragment
    frag.vertices <- [{GeometryPreviewVertex
        xyz=float3( x1, y1, coord), normal=float3(  0, 0, 1),  uv=float2(0,0);
        xyz=float3(x2, y1, coord), normal=float3(  0, 0, 1),  uv=float2(1,0);
        xyz=float3(x2,y2, coord), normal=float3(  0, 0, 1),  uv=float2(1,1);
        xyz=float3( x1,y2, coord), normal=float3(  0, 0, 1),  uv=float2(0,1)
    }]
    apply_gen_direction_tm(plt, frag)
    frag.indices <- [{int 0; 1; 2; 2; 3; 0 }]
    frag.prim = GeometryFragmentType triangles
    gen_bbox(frag)
    return <- frag

def imgui_app(title:string; blk : block)
    if glfwInit()==0
		panic("can't init glfw")
    defer <|
        glfwTerminate()
    glfwInitOpenGL(3,3)
    var window = glfwCreateWindow(1280, 720, "OpenGL - Hello shader clipping", null, null)
    if window==null
		panic("can't create window")
    defer <|
        glfwDestroyWindow(window)
    glfwMakeContextCurrent(window)

    CreateContext(null)
    var io & = unsafe(GetIO())
    StyleColorsDark(null)
    ImGui_ImplGlfw_InitForOpenGL(window, true)
    ImGui_ImplOpenGL3_Init("#version 330")

    var program = create_shader_program(@@vs_preview,@@fs_preview)
    var program_front= create_shader_program(@@vs_preview,@@fs_preview_front)
    var program_caps = create_shader_program(@@vs_preview,@@fs_preview_caps)

    let mesh_file_name = "{get_das_root()}/house.obj"
    var mesh <- load_obj_mesh(mesh_file_name) |> create_geometry_fragment

    let checkerboard_texture = gen_image_checkerboard(16,16, 0xff404040 , 0xff808080) |> create_texture
    glBindTexture(GL_TEXTURE_2D, checkerboard_texture)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    glBindTexture(GL_TEXTURE_2D, 0u)
    var camera_distance = 4.
    var camera_rotation : float
    var camera_position_y : float
    var cursor_position_xy = glfwGetCursorPos(window)
    var cursor_enabled = false
    unsafe
        glfwSetScrollCallback(window) <| @ [[&camera_distance]] ( w, x, y )
            if y>0.0lf
                camera_distance *= 1.1
            else
                camera_distance /= 1.1
        glfwSetCursorPosCallback(window) <| @ [[&cursor_position_xy,&cursor_enabled,&camera_position_y,&camera_rotation,&camera_distance]] ( w, x, y )
            if cursor_enabled
                let cxy = glfwGetCursorPos(window)
                let dxy = cxy - cursor_position_xy
                var display_w, display_h : int
                glfwGetFramebufferSize(window, display_w, display_h)
                camera_position_y += dxy.y * 4. * camera_distance/float(display_h)
                camera_rotation += dxy.x * 720. / float(display_w)
                cursor_position_xy = float2(display_w/2, display_h/2)
                glfwSetCursorPos(window, double(cursor_position_xy.x), double(cursor_position_xy.y))
        glfwSetMouseButtonCallback(window) <| @ [[&cursor_enabled,&cursor_position_xy]] ( w, b, a, m )
            if b==int(GLFW_MOUSE_BUTTON_2)
                cursor_enabled = a==int(GLFW_PRESS)
                if cursor_enabled
                    glfwSetInputMode(window, int(GLFW_CURSOR), int(GLFW_CURSOR_HIDDEN))
                    glfwSetInputMode(window, int(GLFW_RAW_MOUSE_MOTION), int(GLFW_TRUE))
                    var display_w, display_h : int
                    glfwGetFramebufferSize(window, display_w, display_h)
                    cursor_position_xy = float2(display_w/2, display_h/2)
                    glfwSetCursorPos(window, double(cursor_position_xy.x), double(cursor_position_xy.y))
                else
                    glfwSetInputMode(window, int(GLFW_CURSOR), int(GLFW_CURSOR_NORMAL))
                    glfwSetInputMode(window, int(GLFW_RAW_MOUSE_MOTION), int(GLFW_FALSE))
    while glfwWindowShouldClose(window)==0
        glfwPollEvents()

        ImGui_ImplOpenGL3_NewFrame()
        ImGui_ImplGlfw_NewFrame()

        invoke(blk)

        var display_w, display_h : int
        glfwGetFramebufferSize(window, display_w, display_h)
        let aspect = display_h!=0 ? float(display_w)/float(display_h) : 1.
        // render
        glViewport(0, 0, display_w, display_h)
        glClearColor(0.2, 0.2, 0.2, 1.0)
        glClearDepth(1.0lf)
        glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_STENCIL_BUFFER_BIT)
        glEnable(GL_DEPTH_TEST)
        glEnable(GL_CULL_FACE)
        glCullFace(GL_BACK)
        glDepthFunc(GL_LEQUAL)
        let camera_position = float3(0.0 ,camera_position_y+camera_distance*0.5,camera_distance)
        v_view = look_at_rh(camera_position,float3(0,0,0),float3(0,1,0))
        let rot = un_quat_from_unit_vec_ang(normalize(float3(0.,0.,1.)), camera_rotation*PI/180.)
        v_projection = perspective_rh_opengl( 45.0f*PI/180., aspect, 0.1f, 10.0f)
        f_tex := checkerboard_texture
        glUseProgram(program)
        v_model = compose(float4(0.0,0.0,0.0,0.0), rot, float4(1.0))

        let camPos = v_model * float4(camera_position, 1.0)
        f_camera_position_rotated = camPos.xyz

        glEnable(GL_STENCIL_TEST)

        glColorMask(false, false, false, false)
        glDepthMask(false)

        glUseProgram(program_front)
        vs_preview_bind_uniform(program_front)
        fs_preview_front_bind_uniform(program_front)
        //render back faces to stencil with increment it
        glEnable(GL_CULL_FACE);
        glCullFace(GL_FRONT);
        glStencilFunc(GL_ALWAYS, 1, 0xFF)
        glStencilOp(GL_KEEP, GL_KEEP, GL_INCR)
        draw_geometry_fragment(mesh)

        //render front faces to stencil with increment it
        glEnable(GL_CULL_FACE);
        glCullFace(GL_BACK);
        glStencilFunc(GL_ALWAYS, 1, 0xFF)
        glStencilOp(GL_KEEP, GL_KEEP, GL_DECR)
        draw_geometry_fragment(mesh)

        //render caps
        //glDisable(GL_STENCIL_TEST)

        glDisable(GL_CULL_FACE)
        glColorMask(true, true, true, true)
        glDepthMask(true)

        glUseProgram(program_caps)

        glStencilFunc(GL_EQUAL, 1, 0xFF)
        glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP)
        vs_preview_bind_uniform(program_caps)
        fs_preview_caps_bind_uniform(program_caps)

        var planeX <- gen_axis_rect(GenDirection yz, 2.0, -2.0, f_crop_plane_y_max, f_crop_plane_y, -f_crop_plane_x) |> create_geometry_fragment
        draw_geometry_fragment(planeX)
        var planeXMax <- gen_axis_rect(GenDirection yz, 2.0, -2.0, f_crop_plane_y_max,f_crop_plane_y, -f_crop_plane_x_max) |> create_geometry_fragment
        draw_geometry_fragment(planeXMax)

        var planeY <- gen_axis_rect(GenDirection xz, f_crop_plane_x, f_crop_plane_x_max, 2.0, -2.0, f_crop_plane_y) |> create_geometry_fragment
        draw_geometry_fragment(planeY)
        var planeYMax <- gen_axis_rect(GenDirection xz, f_crop_plane_x, f_crop_plane_x_max, 2.0, -2.0, f_crop_plane_y_max) |> create_geometry_fragment
        draw_geometry_fragment(planeYMax)

        glDisable(GL_STENCIL_TEST)
        glDisable(GL_CULL_FACE)

        //render model
        glUseProgram(program)
        vs_preview_bind_uniform(program)
        fs_preview_bind_uniform(program)
        draw_geometry_fragment(mesh)
        
        glUseProgram(0u)

        ImGui_ImplOpenGL3_RenderDrawData(GetDrawData())

        glfwMakeContextCurrent(window)
        glfwSwapBuffers(window)
    //close app
    DestroyContext(null)
    glfwDestroyWindow(window)
    glfwTerminate()

[export]
def test
	imgui_app("imgui") <|
        NewFrame()
        Begin("Crop params");
        SliderFloat("Crop plane X", safe_addr(f_crop_plane_x), -1.0f, 1.0f)
        SliderFloat("Crop plane Xnax", safe_addr(f_crop_plane_x_max), -1.0f, 1.0f)
        SliderFloat("Crop plane Y", safe_addr(f_crop_plane_y), -1.0f, 1.0f)
        SliderFloat("Crop plane Ymax", safe_addr(f_crop_plane_y_max), -1.0f, 1.0f)
        End();
        Render()