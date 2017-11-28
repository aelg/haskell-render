#version 330 core

in vec3 fragmentColor;
in vec3 Position_worldspace;
in vec3 EyeDirection_cameraspace;
in vec3 LightDirection_cameraspace;
in vec3 Normal_cameraspace;

out vec3 color;

void main(){
  // Normal of the computed fragment, in camera space
  vec3 n = normalize( Normal_cameraspace );
  // Direction of the light (from the fragment to the light)
  vec3 l = normalize( LightDirection_cameraspace );

  float cosTheta = clamp(dot( n,l ), 0 ,1);
  float distance = dot(LightDirection_cameraspace, LightDirection_cameraspace)/1.0;

  vec3 AmbientColor = fragmentColor * 0.3;
  vec3 DiffuseColor = (fragmentColor * cosTheta * 0.5)/distance;

  color = DiffuseColor + AmbientColor;


    //vec3 n = normalize(Normal_cameraspace);
    //vec3 l = normalize(LightDirection_cameraspace);
    //vec3 r = normalize(EyeDirection_cameraspace - 2.0*dot(EyeDirection_cameraspace, n)*n);
    //vec3 e = normalize(EyeDirection_cameraspace);
    //float inFlux = clamp( dot( n,l ), 0.0, 1.0 );
    //float reflection = clamp( dot( r,-l ), 0.0, 1.0 );
    //float distance = dot(LightDirection_cameraspace, LightDirection_cameraspace)/1.0;
    //float ambient = 0.3;
    //float diffuse = 1.0*1.0*inFlux;
    //float reflect = 1.0*10.0*pow(reflection,10.0);
    //color = fragmentColor * (ambient + (diffuse+reflect) / distance);
}
